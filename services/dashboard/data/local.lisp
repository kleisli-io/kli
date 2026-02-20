;;;; KLI Dashboard — Local data layer
;;;; Direct calls to task: functions — no Swank RPC.
;;;; Same public API as remote.lisp, mechanical rewrite to in-process calls.

(in-package :kli-dashboard)

;;; ============================================================
;;; ERROR HANDLING
;;; ============================================================

(defmacro with-safe-data (default &body body)
  "Execute BODY for data retrieval, returning DEFAULT on any error.
   Replaces remote-eval-safe pattern: logs error, returns fallback."
  (let ((c (gensym)))
    `(handler-case (progn ,@body)
       (error (,c)
         (format *error-output* "~&[kli-dashboard] ~A~%" ,c)
         ,default))))

;;; ============================================================
;;; CACHE
;;; ============================================================

(defvar *cache* (make-hash-table :test #'equal)
  "TTL cache: key -> (expiry-time . value)")

(defun cache-get (key ttl thunk)
  "Get KEY from cache, recomputing via THUNK if older than TTL seconds."
  (let* ((entry (gethash key *cache*))
         (now (get-universal-time)))
    (if (and entry (< now (car entry)))
        (cdr entry)
        (let ((value (funcall thunk)))
          (setf (gethash key *cache*) (cons (+ now ttl) value))
          value))))

(defun invalidate-cache (&optional key)
  "Clear cache entry for KEY, or all entries if KEY is nil."
  (if key
      (remhash key *cache*)
      (clrhash *cache*)))

;;; ============================================================
;;; ENRICHED TASK DATA
;;; ============================================================

(defun get-enriched-tasks ()
  "All tasks with enriched data: session/obs counts, completion status.
   Cached 60s."
  (cache-get :enriched-tasks 60
    (lambda ()
      (with-safe-data nil
        (let ((result nil))
          (dolist (tk (task:get-cached-task-infos))
            (let* ((id (getf tk :id))
                   (has-events (getf tk :has-events)))
              (if has-events
                  (let* ((view (ignore-errors (task:enriched-task-view id)))
                         (sessions (when view (getf view :sessions)))
                         (base (append tk
                                 (list :session-count
                                       (if (listp sessions) (length sessions) (or sessions 0))
                                       :obs-count
                                       (or (and view (getf view :observations-count)) 0)
                                       :events-count
                                       (or (and view (getf view :events-count)) 0)))))
                    ;; Check completion via CRDT state
                    (ignore-errors
                      (let* ((epath (task:task-events-path id))
                             (log (when (probe-file epath) (task:elog-load epath)))
                             (events (when log (reverse (task:event-log-events log))))
                             (state (when events (task:compute-state events)))
                             (status (when state
                                       (crdt:lww-value (task:task-state-status state)))))
                        (when (and status (string= status "completed"))
                          (setf (getf base :status) :completed))))
                    ;; Check for actual handoff .md files
                    (when (getf tk :has-handoffs)
                      (let* ((dir (task:task-directory id))
                             (hdir (merge-pathnames "handoffs/" dir))
                             (files (when (probe-file hdir)
                                      (directory (merge-pathnames "*.md" hdir)))))
                        (setf (getf base :actual-handoffs) (if files t nil))
                        (setf (getf base :handoff-count) (if files (length files) 0))))
                    (push base result))
                  (push tk result))))
          (nreverse result))))))

(defun get-available-depots ()
  "List of depot name strings (cached 60s)."
  (cache-get :depots 60
    (lambda ()
      (sort (loop for k being the hash-keys of task:*depot-tasks-roots*
                  collect k)
            #'string<))))

(defun get-depot-roots ()
  "Alist of (depot . root-path) for file operations (cached 300s)."
  (cache-get :depot-roots 300
    (lambda ()
      (let ((result nil))
        (maphash (lambda (k _)
                   (declare (ignore _))
                   (let ((root (depot-root-for k)))
                     (when root
                       (push (cons k (namestring root)) result))))
                 task:*depot-tasks-roots*)
        result))))

;;; ============================================================
;;; FILTER HELPERS
;;; ============================================================

(defun filter-by-depot (tasks depot)
  "Filter task list by depot. NIL means all."
  (if depot
      (remove-if-not (lambda (tk) (string-equal depot (getf tk :depot))) tasks)
      tasks))

(defun task-active-p (tk)
  "Is this task event-sourced and not completed?"
  (and (getf tk :has-events)
       (not (eq (getf tk :status) :completed))))

;;; ============================================================
;;; FRONTIER SECTIONS
;;; ============================================================

(defun section-pick-up (&key depot (limit 6))
  "Tasks with actual handoff .md files, sorted by recency."
  (let* ((all (get-enriched-tasks))
         (filtered (filter-by-depot all depot))
         (candidates (remove-if-not
                       (lambda (tk)
                         (and (task-active-p tk)
                              (or (getf tk :actual-handoffs)
                                  (getf tk :has-handoffs))))
                       filtered))
         (sorted (sort (copy-list candidates) #'>
                       :key (lambda (tk) (or (getf tk :latest-mod) 0)))))
    (subseq sorted 0 (min limit (length sorted)))))

(defun section-pick-up-interleaved (&key (limit 6))
  "Pick up across all depots, interleaved so no depot dominates."
  (let* ((depots (get-available-depots))
         (per-depot (mapcar (lambda (d) (section-pick-up :depot d :limit limit))
                            depots))
         (result nil)
         (idx 0))
    (loop while (and (< (length result) limit)
                     (some #'identity per-depot))
          do (let ((depot-tasks (nth (mod idx (length depots)) per-depot)))
               (when depot-tasks
                 (push (pop (nth (mod idx (length depots)) per-depot)) result))
               (incf idx)))
    (nreverse result)))

(defun section-pinned (&key depot)
  "Pinned tasks for a depot, enriched with cached data."
  (let* ((pins (load-pins depot))
         (all (get-enriched-tasks)))
    (loop for pin-id in pins
          for task = (find pin-id all
                          :key (lambda (tk) (getf tk :id))
                          :test #'string=)
          when task collect task)))

(defun section-inbox (&key depot (limit 5))
  "Tasks needing attention: event-sourced, not completed, low session activity."
  (let* ((all (get-enriched-tasks))
         (filtered (filter-by-depot all depot))
         (candidates (remove-if-not
                       (lambda (tk)
                         (and (task-active-p tk)
                              (getf tk :session-count)
                              (<= (getf tk :session-count) 2)
                              (getf tk :obs-count)
                              (> (getf tk :obs-count) 0)))
                       filtered))
         (sorted (sort (copy-list candidates) #'>
                       :key (lambda (tk) (or (getf tk :obs-count) 0)))))
    (subseq sorted 0 (min limit (length sorted)))))

(defun section-ready (&key depot (limit 15))
  "Active tasks grouped by topic."
  (let* ((all (get-enriched-tasks))
         (filtered (filter-by-depot all depot))
         (candidates (remove-if-not
                       (lambda (tk)
                         (and (task-active-p tk)
                              (getf tk :session-count)
                              (> (getf tk :session-count) 0)))
                       filtered))
         (sorted (sort (copy-list candidates) #'>
                       :key (lambda (tk) (or (getf tk :obs-count) 0))))
         (limited (subseq sorted 0 (min limit (length sorted))))
         (groups (make-hash-table :test 'equal)))
    (dolist (tk limited)
      (let ((topic (or (getf tk :topic) "uncategorized")))
        (push tk (gethash topic groups))))
    (let ((result nil))
      (maphash (lambda (k v) (push (cons k (nreverse v)) result)) groups)
      (sort result #'> :key (lambda (g) (length (cdr g)))))))

(defun frontier-data (&key depot)
  "Compute all 4 frontier sections."
  (let* ((all (get-enriched-tasks))
         (filtered (filter-by-depot all depot)))
    (list :depot (or depot "all")
          :available-depots (get-available-depots)
          :pick-up (if depot
                       (section-pick-up :depot depot :limit 6)
                       (section-pick-up-interleaved :limit 6))
          :pinned (section-pinned :depot depot)
          :inbox (section-inbox :depot depot :limit 5)
          :ready (section-ready :depot depot :limit 15)
          :total (length filtered)
          :event-sourced (count-if #'task-active-p filtered)
          :completed (count-if (lambda (tk) (eq (getf tk :status) :completed)) filtered)
          :dormant (count-if (lambda (tk) (not (getf tk :has-events))) filtered))))

;;; ============================================================
;;; PIN STORAGE (per-depot, direct file I/O)
;;; ============================================================

(defun meta-root-for (depot)
  "Get the metadata directory (.kli/ or ace/) for a depot."
  (let ((tasks-root (gethash depot task:*depot-tasks-roots*)))
    (when tasks-root
      (truename (merge-pathnames "../" (pathname tasks-root))))))

(defun depot-root-for (depot)
  "Get the depot root directory (parent of meta dir)."
  (let ((meta (meta-root-for depot)))
    (when meta
      (truename (merge-pathnames "../" meta)))))

(defun pin-file-path (depot)
  "Pin file path: <meta-root>/.dashboard-pins.json."
  (let ((meta (meta-root-for depot)))
    (when meta
      (merge-pathnames ".dashboard-pins.json" meta))))

(defun load-pins (&optional depot)
  "Load pins for a depot. Returns list of task ID strings."
  (let ((path (pin-file-path depot)))
    (when (and path (probe-file path))
      (handler-case
          (with-open-file (s path)
            (yason:parse s))
        (error () nil)))))

(defun save-pins (pins &optional depot)
  "Save pin list for a depot."
  (let ((path (pin-file-path depot)))
    (when path
      (with-open-file (s path :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (yason:encode pins s)
        t))))

(defun pin-task (task-id &optional depot)
  "Pin a task."
  (let ((pins (load-pins depot)))
    (unless (member task-id pins :test #'string=)
      (save-pins (append pins (list task-id)) depot))))

(defun unpin-task (task-id &optional depot)
  "Unpin a task."
  (save-pins (remove task-id (load-pins depot) :test #'string=) depot))

(defun pinned-p (task-id &optional depot)
  "Is task pinned?"
  (member task-id (load-pins depot) :test #'string=))

;;; ============================================================
;;; DISMISSED EDGE SUGGESTIONS
;;; ============================================================

(defun dismissed-edges-path ()
  "Path: <meta-root>/.dashboard-dismissed-edges.json for current depot."
  (let ((meta (meta-root-for nil)))
    (when meta
      (merge-pathnames ".dashboard-dismissed-edges.json" meta))))

(defun load-dismissed-edges ()
  "Load dismissed edge suggestions. Returns list of (from . to) string pairs."
  (let ((path (dismissed-edges-path)))
    (when (and path (probe-file path))
      (handler-case
          (with-open-file (s path)
            (yason:parse s))
        (error () nil)))))

(defun save-dismissed-edges (edges)
  "Save dismissed edge suggestion list."
  (let ((path (dismissed-edges-path)))
    (when path
      (ensure-directories-exist path)
      (with-open-file (s path :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (yason:encode edges s)
        t))))

(defun dismiss-edge-suggestion (from to)
  "Dismiss a suggested edge so it won't appear in health."
  (let* ((dismissed (load-dismissed-edges))
         (key (format nil "~A->~A" from to)))
    (unless (member key dismissed :test #'string=)
      (save-dismissed-edges (append dismissed (list key))))
    (invalidate-cache :health)))

(defun edge-dismissed-p (from to)
  "Check if a suggested edge has been dismissed."
  (let ((key (format nil "~A->~A" from to)))
    (member key (load-dismissed-edges) :test #'string=)))

;;; ============================================================
;;; SCRATCHPAD (per-task, direct file I/O)
;;; ============================================================

(defun split-qualified-id (task-id)
  "Split 'depot:bare-id' into (depot . bare-id). Returns (nil . task-id) if no colon."
  (let ((pos (position #\: task-id)))
    (if pos
        (cons (subseq task-id 0 pos) (subseq task-id (1+ pos)))
        (cons nil task-id))))

(defun scratchpad-file-path (task-id)
  "Get scratchpad file path. Handles __global__ sentinel for depot-level scratchpad."
  (let* ((parts (split-qualified-id task-id))
         (depot (car parts))
         (bare-id (cdr parts))
         (meta (meta-root-for depot))
         (tasks-root (gethash depot task:*depot-tasks-roots*)))
    (when bare-id
      (if (string= bare-id "__global__")
          (when meta
            (merge-pathnames ".dashboard-scratchpad.md" meta))
          (when tasks-root
            (merge-pathnames (format nil "~A/.scratchpad.md" bare-id) tasks-root))))))

(defun load-scratchpad (&optional task-id)
  "Load scratchpad content for a task."
  (let ((path (scratchpad-file-path task-id)))
    (if (and path (probe-file path))
        (with-open-file (s path)
          (let ((content (make-string (file-length s))))
            (read-sequence content s)
            content))
        "")))

(defun save-scratchpad (content &optional task-id)
  "Save scratchpad content for a task."
  (let ((path (scratchpad-file-path task-id)))
    (when path
      (with-open-file (s path :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (write-string content s)
        t))))

;;; ============================================================
;;; HEALTH DATA
;;; ============================================================

(defun get-health-data ()
  "All health diagnostics with item lists (cached 120s)."
  (cache-get :health 120
    (lambda ()
      (with-safe-data nil
        (let* ((stale (task:find-stale-tasks))
               (dead (task:find-dead-ends))
               (orphans (task:find-declared-orphans))
               (claims (task:find-stale-claims))
               (premature (task:find-premature-completions))
               (clusters (task:find-convergent-clusters))
               (suggested-raw (task:find-unlinked-bidirectional-refs))
               (suggested (remove-if
                            (lambda (s)
                              (edge-dismissed-p (getf s :from) (getf s :to)))
                            suggested-raw))
               (frontier (task:find-unexplored-frontier)))
          (list
            :stale-tasks (length stale) :stale-items stale
            :dead-ends (length dead) :dead-items dead
            :orphans (length orphans) :orphan-items orphans
            :stale-claims (length claims) :claim-items claims
            :premature (length premature) :premature-items premature
            :clusters (length clusters) :cluster-items clusters
            :suggested (length suggested) :suggested-items suggested
            :unexplored (length frontier) :frontier-items frontier))))))

;;; ============================================================
;;; ACTIVITY DATA
;;; ============================================================

(defun get-recent-events (&key (limit 200))
  "Get recent events across all tasks (cached 60s)."
  (cache-get (list :events limit) 60
    (lambda ()
      (task:clear-infos-cache)
      (with-safe-data nil
        (let ((events nil))
          (dolist (info (task:get-cached-task-infos))
            (let* ((id (getf info :id))
                   (epath (task:task-events-path id)))
              (when (and epath (probe-file epath))
                (ignore-errors
                  (let ((log (task:elog-load epath)))
                    (dolist (ev (task:event-log-events log))
                      (push (list :task id
                                  :type (task:event-type ev)
                                  :timestamp (task:event-timestamp ev)
                                  :session (task:event-session ev)
                                  :data (task:event-data ev))
                            events)))))))
          (setf events (sort events #'> :key (lambda (e) (or (getf e :timestamp) 0))))
          (subseq events 0 (min limit (length events))))))))

(defun get-all-events ()
  "Get all events across all tasks, sorted newest first (cached 60s).
   Unlike get-recent-events, returns the full set with no limit."
  (cache-get :all-events 60
    (lambda ()
      (task:clear-infos-cache)
      (with-safe-data nil
        (let ((events nil))
          (dolist (info (task:get-cached-task-infos))
            (let* ((id (getf info :id))
                   (epath (task:task-events-path id)))
              (when (and epath (probe-file epath))
                (ignore-errors
                  (let ((log (task:elog-load epath)))
                    (dolist (ev (task:event-log-events log))
                      (push (list :task id
                                  :type (task:event-type ev)
                                  :timestamp (task:event-timestamp ev)
                                  :session (task:event-session ev)
                                  :data (task:event-data ev))
                            events)))))))
          (sort events #'> :key (lambda (e) (or (getf e :timestamp) 0))))))))

(defun filter-events-by-depot (events depot)
  "Filter events by depot prefix on :task field. NIL depot = no filter."
  (if depot
      (let ((prefix (concatenate 'string depot ":")))
        (remove-if-not
         (lambda (e)
           (let ((id (getf e :task)))
             (and id (>= (length id) (length prefix))
                  (string= prefix id :end2 (length prefix)))))
         events))
      events))

(defun get-events-range (events offset limit &optional category)
  "Return OFFSET..OFFSET+LIMIT slice of EVENTS, optionally filtered by CATEGORY string.
   CATEGORY nil or \"all\" means no filter."
  (let ((filtered (if (or (null category) (string-equal category "all"))
                      events
                      (remove-if-not
                       (lambda (e) (string= category (event-category (getf e :type))))
                       events))))
    (when (< offset (length filtered))
      (subseq filtered offset (min (+ offset limit) (length filtered))))))

;;; ============================================================
;;; STATS DATA
;;; ============================================================

(defun get-stats-data (&optional depot)
  "Aggregate statistics for the stats page (cached 60s).
   Returns plist with event counts, per-day breakdown, per-type, per-depot,
   task status counts, top active tasks, and session summary."
  (cache-get (list :stats depot) 60
    (lambda ()
      (with-safe-data nil
        (let ((per-day (make-hash-table :test #'equal))
              (per-type (make-hash-table :test #'eq))
              (per-depot (make-hash-table :test #'equal))
              (per-task (make-hash-table :test #'equal))
              (per-day-creates (make-hash-table :test #'equal))
              (per-day-completes (make-hash-table :test #'equal))
              (total-events 0))
          ;; Scan all events
          (dolist (info (task:get-cached-task-infos))
            (when (getf info :has-events)
              (let* ((id (getf info :id))
                     (task-depot (getf info :depot))
                     (epath (task:task-events-path id)))
                (when (and epath (probe-file epath)
                           (or (null depot) (string-equal depot task-depot)))
                  (ignore-errors
                    (let ((log (task:elog-load epath)))
                      (dolist (ev (task:event-log-events log))
                        (incf total-events)
                        (incf (gethash task-depot per-depot 0))
                        (incf (gethash (task:event-type ev) per-type 0))
                        (incf (gethash id per-task 0))
                        (let ((ts (task:event-timestamp ev))
                              (etype (task:event-type ev)))
                          (when (and ts (numberp ts) (> ts 0))
                            (multiple-value-bind (s m h day month year)
                                (decode-universal-time ts)
                              (declare (ignore s m h))
                              (let ((date-key (format nil "~D-~2,'0D-~2,'0D"
                                                      year month day)))
                                (incf (gethash date-key per-day 0))
                                (when (eq etype :task.create)
                                  (incf (gethash date-key per-day-creates 0)))
                                (when (eq etype :task.update-status)
                                  (let ((edata (task:event-data ev)))
                                    (when (and edata
                                               (string= "completed"
                                                        (getf edata :status)))
                                      (incf (gethash date-key
                                                     per-day-completes 0))))))))))))))))
          ;; Task status counts (from enriched tasks)
          (let* ((tasks (get-enriched-tasks))
                 (filtered (if depot (filter-by-depot tasks depot) tasks))
                 (completed (count-if (lambda (tk) (eq (getf tk :status) :completed)) filtered))
                 (active (count-if (lambda (tk) (task-active-p tk)) filtered))
                 (dormant (count-if (lambda (tk) (not (getf tk :has-events))) filtered)))
            ;; Top tasks by event count
            (let ((top-tasks nil))
              (maphash (lambda (id count)
                         (push (cons id count) top-tasks))
                       per-task)
              (setf top-tasks (subseq (sort top-tasks #'> :key #'cdr)
                                      0 (min 10 (length top-tasks))))
              ;; Session stats
              (let* ((fps (ignore-errors (task:cached-session-fingerprints)))
                     (session-count (if fps (hash-table-count fps) 0))
                     (builders 0) (observers 0))
                (when fps
                  (maphash (lambda (sid fp)
                             (declare (ignore sid))
                             (if (eq :builder (ignore-errors
                                                (task:classify-session-archetype fp)))
                                 (incf builders) (incf observers)))
                           fps))
                ;; Events per day sorted, filtered to last 60 days
                (let ((daily nil)
                      (cutoff (multiple-value-bind (s min h day month year)
                                  (decode-universal-time (- (get-universal-time) (* 60 60 24 60)))
                                (declare (ignore s min h))
                                (format nil "~4,'0D-~2,'0D-~2,'0D" year month day))))
                  (maphash (lambda (date count) (push (cons date count) daily)) per-day)
                  (setf daily (sort daily #'string< :key #'car))
                  (setf daily (remove-if (lambda (entry) (string< (car entry) cutoff)) daily))
                  ;; Cumulative task series
                  (let ((acc-c 0) (acc-co 0)
                        (cum-c nil) (cum-co nil))
                    (dolist (entry daily)
                      (push (cons (car entry)
                                  (incf acc-c (gethash (car entry)
                                                       per-day-creates 0)))
                            cum-c)
                      (push (cons (car entry)
                                  (incf acc-co (gethash (car entry)
                                                        per-day-completes 0)))
                            cum-co))
                    (setf cum-c (nreverse cum-c)
                          cum-co (nreverse cum-co))
                  ;; Event types sorted
                  (let ((types nil))
                    (maphash (lambda (type count) (push (cons type count) types)) per-type)
                    (setf types (sort types #'> :key #'cdr))
                    ;; Depot breakdown sorted
                    (let ((depots nil))
                      (maphash (lambda (dep count) (push (cons dep count) depots)) per-depot)
                      (setf depots (sort depots #'> :key #'cdr))
                      (list :total-events total-events
                            :total-tasks (length filtered)
                            :completed completed :active active :dormant dormant
                            :daily daily
                            :cumulative-creates cum-c
                            :cumulative-completes cum-co
                            :types types
                            :depots depots
                            :top-tasks top-tasks
                            :session-count session-count
                            :builders builders :observers observers)))))))))))))

;;; ============================================================
;;; SESSION DATA
;;; ============================================================

(defun get-session-data (&optional depot)
  "Session inventory for the sessions page (cached 30s).
   Returns plist with :active sessions (recent, no leave) and :history (all sessions)."
  (cache-get (list :sessions depot) 30
    (lambda ()
      (with-safe-data nil
        (let ((sessions (make-hash-table :test #'equal))
              (now (get-universal-time)))
          ;; Scan all events to build session profiles
          (dolist (info (task:get-cached-task-infos))
            (when (getf info :has-events)
              (let* ((id (getf info :id))
                     (task-depot (getf info :depot))
                     (epath (task:task-events-path id)))
                (when (and epath (probe-file epath)
                           (or (null depot) (string-equal depot task-depot)))
                  (ignore-errors
                    (let ((log (task:elog-load epath)))
                      (dolist (ev (task:event-log-events log))
                        (let ((sid (task:event-session ev))
                              (ts (task:event-timestamp ev))
                              (type (task:event-type ev)))
                          (when sid
                            (let ((entry (gethash sid sessions)))
                              (unless entry
                                (setf entry (list :first-seen ts :last-seen 0
                                                  :event-count 0
                                                  :tasks nil
                                                  :depots nil
                                                  :left-at nil))
                                (setf (gethash sid sessions) entry))
                              (incf (getf entry :event-count))
                              (when (< ts (getf entry :first-seen))
                                (setf (getf entry :first-seen) ts))
                              (when (> ts (getf entry :last-seen))
                                (setf (getf entry :last-seen) ts))
                              (unless (member id (getf entry :tasks) :test #'string=)
                                (push id (getf entry :tasks)))
                              (unless (member task-depot (getf entry :depots) :test #'string=)
                                (push task-depot (getf entry :depots)))
                              (when (eq type :session.leave)
                                (setf (getf entry :left-at) ts))))))))))))
          ;; Split into active vs history
          (let ((active nil)
                (history nil)
                (threshold (- now 3600)))
            (maphash
              (lambda (sid data)
                (let* ((last (getf data :last-seen))
                       (left (getf data :left-at))
                       (is-active (and (> last threshold)
                                       (or (null left) (< left last))))
                       (duration (- last (getf data :first-seen)))
                       (entry (list :session sid
                                    :first-seen (getf data :first-seen)
                                    :last-seen last
                                    :ago-seconds (- now last)
                                    :duration duration
                                    :event-count (getf data :event-count)
                                    :task-count (length (getf data :tasks))
                                    :tasks (getf data :tasks)
                                    :depots (getf data :depots)
                                    :left (not (null left)))))
                  (if is-active
                      (push entry active)
                      (push entry history))))
              sessions)
            (list :active (sort active #'< :key (lambda (e) (getf e :ago-seconds)))
                  :active-count (length active)
                  :history (sort (subseq (sort history #'> :key (lambda (e) (getf e :last-seen)))
                                         0 (min 50 (length history)))
                                 #'> :key (lambda (e) (getf e :last-seen)))
                  :total-sessions (hash-table-count sessions))))))))

;;; ============================================================
;;; GRAPH DATA
;;; ============================================================

(defun get-graph-json ()
  "Get full graph as JSON string for D3 (cached 60s)."
  (cache-get :graph-json 60
    (lambda ()
      (with-safe-data "{\"nodes\":[],\"edges\":[]}"
        (task:graph-to-dashboard-json
          (task:get-cached-multi-depot-graph)
          (task:get-cached-task-infos))))))

;;; ============================================================
;;; TASK DETAIL DATA
;;; ============================================================

(defun task-detail-data (task-id)
  "Full task detail: description, goals, phase, status, children, neighbors,
   observations, sessions, handoffs, events."
  (with-safe-data nil
    (let* ((id task-id)
           (state (ignore-errors
                    (let* ((epath (task:task-events-path id))
                           (log (when (probe-file epath) (task:elog-load epath)))
                           (events (when log (reverse (task:event-log-events log)))))
                      (when events (task:compute-state events)))))
           (desc (when state (crdt:lww-value (task:task-state-description state))))
           (metadata (when state (task:task-state-metadata state)))
           (goals-json (when metadata (crdt:lwwm-get metadata "goals")))
           (goals (when goals-json (ignore-errors (task:try-parse-json-array goals-json))))
           (phase (when metadata (crdt:lwwm-get metadata "phase")))
           (tags (when metadata (crdt:lwwm-get metadata "tags")))
           (status (when state
                     (crdt:lww-value (task:task-state-status state))))
           (children (ignore-errors (task:task-children id)))
           (view (ignore-errors (task:enriched-task-view id)))
           (sessions (when view (getf view :sessions)))
           (recent-obs (when view (getf view :recent-observations)))
           (obs-count (or (and view (getf view :observations-count)) 0))
           (events-count (or (and view (getf view :events-count)) 0))
           ;; Graph neighbors
           (graph (ignore-errors (task:get-cached-multi-depot-graph)))
           (qid (let ((p (position #\: id)))
                  (if p id (format nil "core:~A" id))))
           (fwd (when graph (gethash qid (task:task-graph-forward graph))))
           (rev (when graph (gethash qid (task:task-graph-reverse graph))))
           (neighbors (append
                        (mapcar (lambda (e) (list :id (first e) :edge (second e) :dir :outgoing)) fwd)
                        (mapcar (lambda (e) (list :id (first e) :edge (second e) :dir :incoming)) rev)))
           ;; Handoffs
           (dir (task:task-directory id))
           (hdir (merge-pathnames "handoffs/" dir))
           (handoff-files (when (probe-file hdir)
                            (sort (directory (merge-pathnames "*.md" hdir))
                                  #'string> :key #'namestring)))
           (handoffs (loop for f in (subseq handoff-files 0 (min 10 (length handoff-files)))
                           collect (list :name (pathname-name f)
                                         :path (enough-namestring f dir)))))
      (list :description desc
            :goals goals
            :phase phase
            :tags tags
            :status (or status "active")
            :children (mapcar #'namestring children)
            :neighbors neighbors
            :sessions sessions
            :recent-obs recent-obs
            :obs-count obs-count
            :events-count events-count
            :handoff-count (length handoff-files)
            :handoffs handoffs))))

;;; ============================================================
;;; EXPAND PANEL DATA
;;; ============================================================

(defun task-expand-data (task-id)
  "Gather data for expand panel. Returns plist."
  (with-safe-data nil
    (let* ((id task-id)
           (state (ignore-errors
                    (let* ((events-path (task:task-events-path id))
                           (log (when (probe-file events-path)
                                  (task:elog-load events-path)))
                           (events (when log (reverse (task:event-log-events log)))))
                      (when events (task:compute-state events)))))
           (desc (when state
                   (crdt:lww-value (task:task-state-description state))))
           (metadata (when state (task:task-state-metadata state)))
           (goals-json (when metadata (crdt:lwwm-get metadata "goals")))
           (goals (when goals-json
                    (ignore-errors (task:try-parse-json-array goals-json))))
           (phase (when metadata (crdt:lwwm-get metadata "phase")))
           (children (ignore-errors (task:task-children id)))
           (dir (task:task-directory id))
           (hdir (merge-pathnames "handoffs/" dir))
           (handoff-files (when (probe-file hdir)
                            (sort (directory (merge-pathnames "*.md" hdir))
                                  #'string> :key #'namestring)))
           (latest-handoff (first handoff-files))
           (handoff-content
             (when latest-handoff
               (ignore-errors
                 (with-open-file (s latest-handoff)
                   (let ((first-line (read-line s nil)))
                     (when (and first-line
                                (string= (string-trim '(#\Space #\Tab) first-line) "---"))
                       (loop for l = (read-line s nil)
                             while (and l (not (string= (string-trim '(#\Space #\Tab) l) "---"))))))
                   (let ((buf (make-string-output-stream)))
                     (loop for l = (read-line s nil)
                           for total = 0 then (+ total (length l) 1)
                           while (and l (< total 800))
                           do (write-line l buf))
                     (get-output-stream-string buf))))))
           (view (ignore-errors (task:enriched-task-view id)))
           (recent-obs (when view (getf view :recent-observations))))
      (list :description desc
            :goals goals
            :phase phase
            :children (mapcar #'namestring children)
            :handoff-content handoff-content
            :recent-obs recent-obs))))

;;; ============================================================
;;; HANDOFF DATA
;;; ============================================================

(defun handoff-content (task-id handoff-name)
  "Load handoff content. Returns markdown text (up to 4000 chars)."
  (with-safe-data nil
    (let* ((dir (task:task-directory task-id))
           (path (merge-pathnames (format nil "handoffs/~A.md" handoff-name) dir)))
      (when (probe-file path)
        (with-open-file (s path)
          (let ((buf (make-string (min 4000 (file-length s)))))
            (let ((n (read-sequence buf s)))
              (subseq buf 0 n))))))))

;;; ============================================================
;;; PLAN DATA
;;; ============================================================

(defun plan-index-data ()
  "Find all tasks that have children (plan structure). Cached 60s."
  (cache-get :plan-index 60
    (lambda ()
      (with-safe-data nil
        (let ((result nil))
          (dolist (tk (task:get-cached-task-infos))
            (let ((id (getf tk :id)))
              (ignore-errors
                (let ((children (task:task-children id)))
                  (when children
                    (let* ((states (task:load-child-states children))
                           (done 0) (total (length children)))
                      (dolist (c children)
                        (let ((state (gethash c states)))
                          (when (and state
                                     (string-equal "completed"
                                       (or (crdt:lww-value (task:task-state-status state)) "")))
                            (incf done))))
                      (push (list :id id
                                  :display-name (or (getf tk :display-name) id)
                                  :date (getf tk :date)
                                  :phases total
                                  :done done
                                  :topic (getf tk :topic))
                            result)))))))
          (nreverse result))))))

(defun plan-detail-data (task-id)
  "Full plan data for TASK-ID: children, sorted phases, done/ready counts."
  (with-safe-data nil
    (let* ((id task-id)
           (children (ignore-errors (task:task-children id))))
      (if (not children)
          (list :children nil)
          (let* ((states (task:load-child-states children))
                 (sorted (task:stable-topo-sort children states))
                 (frontier (ignore-errors (task:plan-frontier id)))
                 (frontier-set (make-hash-table :test #'equal))
                 (done 0)
                 (total (length sorted))
                 (phases nil))
            (dolist (f (or frontier nil))
              (setf (gethash f frontier-set) t))
            (dolist (child-id sorted)
              (let* ((state (gethash child-id states))
                     (details (ignore-errors
                                (task:extract-phase-details
                                  child-id
                                  (ignore-errors
                                    (let ((ep (task:task-events-path child-id)))
                                      (when (probe-file ep)
                                        (reverse (task:event-log-events
                                                   (task:elog-load ep))))))
                                  state frontier-set)))
                     (status-str (or (getf details :status) "pending"))
                     (is-ready (getf details :ready))
                     (display (getf details :display))
                     (description (getf details :description))
                     (observations (getf details :observations))
                     (artifacts (getf details :artifacts))
                     (deps (getf details :deps))
                     (edges (getf details :edges))
                     (sessions (or (getf details :sessions) 0)))
                (when (string-equal status-str "completed") (incf done))
                (push (list :id child-id
                            :status status-str
                            :ready is-ready
                            :display (or display child-id)
                            :description (when (and description (> (length description) 0))
                                           (subseq description 0
                                                   (min 200 (length description))))
                            :obs-count (length observations)
                            :artifact-count (length artifacts)
                            :session-count sessions
                            :deps deps
                            :edges (mapcar (lambda (e)
                                             (list :type (getf e :type)
                                                   :target (getf e :target)))
                                           (remove-if (lambda (e)
                                                        (string-equal (getf e :type)
                                                                      "depends-on"))
                                                      edges)))
                      phases)))
            (list :children (mapcar #'namestring children)
                  :sorted (nreverse phases)
                  :total total
                  :done done
                  :ready-count (length frontier)
                  :display-name
                  (ignore-errors
                    (let* ((ep (task:task-events-path id))
                           (log (when (probe-file ep) (task:elog-load ep)))
                           (evts (when log (reverse (task:event-log-events log))))
                           (st (when evts (task:compute-state evts)))
                           (meta (when st (task:task-state-metadata st))))
                      (when meta (crdt:lwwm-get meta "display-name"))))))))))

;;; ============================================================
;;; PLAN RICH JSON (for D3 DAG)
;;; ============================================================

(defun plan-rich-json (task-id)
  "Get plan-to-rich-json output for D3 DAG visualization. Returns JSON string."
  (with-safe-data "{}"
    (task:plan-to-rich-json task-id)))

;;; ============================================================
;;; CLUSTER DATA
;;; ============================================================

(defun cluster-data (&optional depot)
  "Build enriched cluster data. Cached 60s.
   Returns plist with :groups, :singletons, :total, :cluster-count, :singleton-count."
  (cache-get (list :clusters depot) 60
    (lambda ()
      (with-safe-data nil
        (let* ((all-tasks (if depot
                              (remove-if-not
                                (lambda (tk) (string-equal depot (getf tk :depot)))
                                (get-enriched-tasks))
                              (get-enriched-tasks)))
               (clusters (make-hash-table :test #'equal))
               (graph (task:get-cached-multi-depot-graph))
               (forward (task:task-graph-forward graph)))
          ;; Build clusters by topic
          (dolist (tk all-tasks)
            (let ((topic (or (getf tk :topic) "uncategorized")))
              (push tk (gethash topic clusters))))
          ;; Separate groups and singletons
          (let ((groups nil) (singletons nil))
            (maphash
              (lambda (topic tasks)
                (if (> (length tasks) 1)
                    (let* ((ids (mapcar (lambda (tk) (getf tk :id)) tasks))
                           (id-set (make-hash-table :test #'equal))
                           (internal 0) (cross 0))
                      (dolist (id ids) (setf (gethash id id-set) t))
                      (dolist (id ids)
                        (dolist (entry (gethash id forward))
                          (if (gethash (first entry) id-set)
                              (incf internal) (incf cross))))
                      (let* ((n (length tasks))
                             (density (if (> n 1)
                                         (float (/ internal (* n (1- n))))
                                         0.0))
                             (completed (count-if
                                          (lambda (tk)
                                            (eq (getf tk :status) :completed))
                                          tasks))
                             (completion (float (/ completed n))))
                        (push (list :topic topic
                                    :task-count n
                                    :tasks (mapcar
                                             (lambda (tk)
                                               (list :id (getf tk :id)
                                                     :display-name
                                                     (or (getf tk :display-name)
                                                         (getf tk :bare-id)
                                                         (getf tk :id))
                                                     :status (getf tk :status)
                                                     :obs-count (or (getf tk :obs-count) 0)
                                                     :session-count (or (getf tk :session-count) 0)
                                                     :date (getf tk :date)))
                                             tasks)
                                    :density density
                                    :completion completion
                                    :internal-edges internal
                                    :cross-links cross)
                              groups)))
                    (push (list :id (getf (first tasks) :id)
                                :display-name (or (getf (first tasks) :display-name)
                                                  (getf (first tasks) :bare-id))
                                :status (getf (first tasks) :status))
                          singletons)))
              clusters)
            (list :groups (sort groups (lambda (a b)
                                         (> (getf a :task-count)
                                            (getf b :task-count))))
                  :singletons singletons
                  :total (length all-tasks)
                  :cluster-count (length groups)
                  :singleton-count (length singletons))))))))

;;; ============================================================
;;; EVENT EMISSION (for mutation routes)
;;; ============================================================

(defun emit-task-event (task-id event-type data)
  "Emit an event to a task's event log and clear caches."
  (let* ((path (task:task-events-path task-id))
         (ev (task:make-event
               :id (format nil "dashboard-~A" (get-universal-time))
               :timestamp (get-universal-time)
               :session "kli-dashboard"
               :clock (crdt:make-vector-clock)
               :type event-type
               :data data)))
    (task:elog-append-event path ev)
    (task:clear-graph-cache)
    (task:clear-infos-cache)
    (invalidate-cache)
    t))

;;; ============================================================
;;; HELPERS
;;; ============================================================

(defun css-safe-id (id)
  "Replace colons with -- for CSS/HTMX selector safety."
  (with-output-to-string (s)
    (loop for c across id
          do (if (char= c #\:)
                 (write-string "--" s)
                 (write-char c s)))))

(defun humanize-task-name (id)
  "Convert task ID to human-readable name."
  (let* ((bare (if (find #\: id)
                   (subseq id (1+ (position #\: id)))
                   id))
         (trimmed (if (and (>= (length bare) 11)
                           (digit-char-p (char bare 0)))
                      (subseq bare 11)
                      bare)))
    (substitute #\Space #\- (string-capitalize trimmed))))

(defun extract-topic (bare-id)
  "Extract topic cluster from bare task ID (strip date prefix, take meaningful words)."
  (let* ((name (if (and (> (length bare-id) 11)
                        (digit-char-p (char bare-id 0))
                        (char= (char bare-id 4) #\-)
                        (char= (char bare-id 7) #\-))
                   (subseq bare-id 11)
                   bare-id))
         (parts (uiop:split-string name :separator "-"))
         (noise '("and" "for" "the" "with" "from" "into" "implementation"
                  "exploration" "research" "fix" "update" "add" "new")))
    (let ((meaningful (loop for p in parts
                           for i from 0
                           while (and (< i 2)
                                      (not (member p noise :test #'string-equal)))
                           collect p)))
      (if meaningful
          (format nil "~{~A~^-~}" meaningful)
          (or (first parts) "unknown")))))

(defun topic-color (topic)
  "Generate consistent HSL color string for a topic."
  (if (and topic (stringp topic) (> (length topic) 0))
      (format nil "hsl(~D, 55%, 65%)" (mod (sxhash topic) 360))
      "var(--color-accent)"))

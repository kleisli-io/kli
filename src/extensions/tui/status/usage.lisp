(in-package #:kli/tui/status)

(defstruct (context-usage-readout (:constructor %make-context-usage-readout)
                                  (:conc-name readout-))
  "The footer's single source of truth, a pure function of its arguments. TOKENS
   is the numerator for ctx%/window, or NIL when unknown. CONTEXT-WINDOW is the
   denominator, or NIL when the model declares none. PERCENT is a single-float
   0..100+, or NIL when unknown. COST is NIL unless a provider reports it."
  tokens
  context-window
  percent
  model-id provider thinking
  input output cache-read cache-write total
  cost)


(defun current-context-window (registry)
  "Context window of REGISTRY's current model selection, or NIL."
  (selection-context-window registry (current-model-selection registry)))

(defun trailing-token-estimate (messages)
  "chars/4 estimate of the messages after the last assistant message -- the
trailing turn not yet reflected in model-reported usage. With no assistant
message present, the whole list is trailing."
  (let ((cut (position :assistant messages :key #'message-role :from-end t)))
    (loop for m in (if cut (nthcdr (1+ cut) messages) messages)
          sum (estimate-message-tokens m))))

(defun usage-unknown-after-compaction-p (entries)
  "T when the latest compaction has no assistant message after it: the stored
usage still reflects pre-compaction context, so the count is unknown until the
next response (pi renders this as `?`)."
  (let* ((vec (coerce entries 'vector))
         (last-compaction (position-if (lambda (e) (typep e 'compaction-entry))
                                       vec :from-end t)))
    (and last-compaction
         (loop for i from (1+ last-compaction) below (length vec)
               for e = (aref vec i)
               never (and (message-entry-p e)
                          (eq (message-role (entry-message e)) :assistant))))))

(defun compute-context-usage (&key total trailing-tokens context-window
                                   model-id provider thinking
                                   input output cache-read cache-write cost
                                   (usage-known-p t))
  "Pure read-model. TOKENS = TOTAL + TRAILING-TOKENS, and PERCENT =
100*TOKENS/CONTEXT-WINDOW. TOKENS and PERCENT are NIL when CONTEXT-WINDOW is
missing/non-positive or USAGE-KNOWN-P is NIL. CONTEXT-WINDOW is still reported so
the footer can show `?/window`."
  (let* ((window (and context-window (plusp context-window) context-window))
         (tokens (when (and usage-known-p window)
                   (+ (or total 0) (or trailing-tokens 0))))
         (percent (when (and tokens window)
                    (* 100.0 (/ tokens window)))))
    (%make-context-usage-readout
     :tokens tokens
     :context-window window
     :percent percent
     :model-id model-id :provider provider :thinking thinking
     :input input :output output
     :cache-read cache-read :cache-write cache-write
     :total total :cost cost)))

(defun session-context-usage-readout (registry selection usage messages entries)
  "Assemble a readout from resolved session pieces: SELECTION (the active mode's
live model, or NIL), REGISTRY (only for the model's declared window), the latest
reported USAGE (a context-usage or NIL), and the in-context MESSAGES / ENTRIES
(trailing estimate + compaction validity)."
  (compute-context-usage
   :context-window (selection-context-window registry selection)
   :model-id (and selection (model-selection-model-id selection))
   :provider (and selection (model-selection-provider-id selection))
   :thinking (and selection (model-selection-option-value selection "reasoning-effort"))
   :total (and usage (usage-total-tokens usage))
   :input (and usage (usage-input-tokens usage))
   :output (and usage (usage-output-tokens usage))
   :cache-read (and usage (usage-cache-read-tokens usage))
   :cache-write (and usage (usage-cache-write-tokens usage))
   :trailing-tokens (trailing-token-estimate messages)
   :usage-known-p (not (usage-unknown-after-compaction-p entries))))

(defun humanize-token-count (n)
  "N as a compact count: a bare integer below 1000, else thousands with one
decimal (dropped when whole). NIL renders as `?` -- the count is unknown."
  (cond ((null n) "?")
        ((< n 1000) (format nil "~D" n))
        (t (let ((k (/ n 1000.0)))
             (if (= k (ffloor k))
                 (format nil "~Dk" (truncate k))
                 (format nil "~,1Fk" k))))))

(defun format-context-usage-readout (readout)
  "READOUT as one footer line -- `model · thinking · tokens/window (pct)` -- or
NIL when there is no readout at all. A readout with no model selected still
renders, anchored by `none`, so the footer is present before a model is chosen.
Tokens read as `?` when unknown. Thinking is omitted when absent, and the
window/percent group is dropped when no window is declared."
  (when readout
    (let ((parts '()))
      (push (or (readout-model-id readout) "none") parts)
      (let ((thinking (readout-thinking readout)))
        (when thinking (push (string-downcase (string thinking)) parts)))
      (let ((window (readout-context-window readout))
            (tokens (readout-tokens readout))
            (percent (readout-percent readout)))
        (cond
          (window
           (push (format nil "~A/~A~:[~; (~D%)~]"
                         (humanize-token-count tokens)
                         (humanize-token-count window)
                         percent (and percent (round percent)))
                 parts))
          (tokens
           (push (humanize-token-count tokens) parts))))
      (when parts
        (format nil "~{~A~^ · ~}" (nreverse parts))))))

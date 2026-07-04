(in-package #:kli/config)

(defun format-settings-table (settings)
  (if (and (hash-table-p settings) (plusp (hash-table-count settings)))
      (com.inuoe.jzon:stringify settings :pretty t)
      "{}"))

(defun format-config-path (path present)
  (if path
      (format nil "~A~:[ (absent)~;~]" (namestring path) present)
      "(none)"))

(defun format-config-summary (summary)
  (with-output-to-string (out)
    (format out "Config directories:")
    (format out "~%  global   ~A" (namestring (getf summary :global-dir)))
    (format out "~%  project  ~A"
            (let ((dir (getf summary :project-dir)))
              (if dir (namestring dir) "(none)")))
    (format out "~%Settings files:")
    (format out "~%  global   ~A"
            (format-config-path (getf summary :global-settings-path)
                                (getf summary :global-settings-present)))
    (format out "~%  project  ~A"
            (format-config-path (getf summary :project-settings-path)
                                (getf summary :project-settings-present)))
    (let ((kinds (getf summary :resource-kinds)))
      (when kinds
        (format out "~%Resource kinds:")
        (loop for (kind . subdir) in kinds
              do (format out "~%  ~(~A~) -> ~A"
                         kind (namestring subdir)))))
    (format out "~%Merged settings:~%~A"
            (format-settings-table (getf summary :settings)))))

(defun run-settings-command (command arguments context &key call-id on-update)
  (declare (ignore command arguments call-id on-update))
  (let* ((service (find-config-service context))
         (summary (format-config-summary (config-summary service)))
         (protocol (active-protocol context))
         (report (and protocol
                      (format-extension-settings-report
                       (extension-settings-report
                        protocol (config-service-settings service))))))
    (reply (if report
               (format nil "~A~%~A" summary report)
               summary))))

(in-package #:kli/auth/core)

(defvar *credentials-path* nil
  "Override for the credentials file. NIL resolves to the XDG default.")

(defvar *credentials-write-mutex* (sb-thread:make-mutex :name "kli-credentials")
  "Serializes the load-modify-save of the credentials file. save-credentials is
already crash-safe via temp + atomic rename, so the file is never torn; the lock
closes the lost-update window when two in-image refreshes race the read part.
The file is one shared resource, so a single image-global lock covers all paths.")

(defun credentials-path ()
  (or *credentials-path*
      (uiop:xdg-config-home "kli" "credentials.json")))

(defun alist-to-object (alist)
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key . value) in alist
          do (setf (gethash key object) value))
    object))

(defun load-credentials (&optional (path (credentials-path)))
  "Provider-id -> record object. Empty table when the file is absent."
  (if (probe-file path)
      (let ((parsed (com.inuoe.jzon:parse (uiop:read-file-string path))))
        (if (hash-table-p parsed) parsed (make-hash-table :test #'equal)))
      (make-hash-table :test #'equal)))

(defun save-credentials (table &optional (path (credentials-path)))
  "Write TABLE to a same-directory temp file and rename it over PATH, so a
crash mid-write cannot truncate the stored credentials. umask 077 keeps the
temp at 0600 with no world-readable window; the explicit chmod covers a
pre-existing temp left under looser permissions."
  (let ((temp (make-pathname :type "tmp" :defaults path))
        (previous-umask (sb-posix:umask #o077)))
    (unwind-protect
         (progn
           (ensure-directories-exist path)
           (with-open-file (out temp :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create
                                     :external-format :utf-8)
             (com.inuoe.jzon:stringify table :stream out :pretty t))
           (sb-posix:chmod temp #o600)
           (rename-file temp path))
      (sb-posix:umask previous-umask)))
  path)

(defun read-credential-record (provider-id &optional (path (credentials-path)))
  (gethash (normalize-provider-id provider-id) (load-credentials path)))

(defun write-credential-record (provider-id record
                                &optional (path (credentials-path)))
  (sb-thread:with-mutex (*credentials-write-mutex*)
    (let ((table (load-credentials path)))
      (setf (gethash (normalize-provider-id provider-id) table) record)
      (save-credentials table path)))
  record)

(defun delete-credential-record (provider-id &optional (path (credentials-path)))
  (sb-thread:with-mutex (*credentials-write-mutex*)
    (let ((table (load-credentials path)))
      (remhash (normalize-provider-id provider-id) table)
      (save-credentials table path)))
  nil)

(defun static-credential-record (key-string)
  (alist-to-object (list (cons "kind" "static")
                         (cons "key" key-string))))

(defun oauth-credential-record (&key access refresh expires account-id)
  (alist-to-object
   (append (list (cons "kind" "oauth"))
           (when access (list (cons "access" access)))
           (when refresh (list (cons "refresh" refresh)))
           (when expires (list (cons "expires" expires)))
           (when account-id (list (cons "account-id" account-id))))))

(defun credential-record-kind (record)
  (let ((kind (and record (gethash "kind" record))))
    (cond ((null kind) nil)
          ((string-equal kind "static") :static)
          ((string-equal kind "oauth") :oauth)
          (t nil))))

(defun credential-reference-from-record (provider-id record
                                         &key (store-path (credentials-path))
                                              scope metadata)
  (ecase (credential-record-kind record)
    (:static
     (make-static-credential-reference provider-id (gethash "key" record)
                                        :scope scope :metadata metadata))
    (:oauth
     (make-oauth-credential-reference provider-id
                                      :access (gethash "access" record)
                                      :refresh (gethash "refresh" record)
                                      :expires (or (gethash "expires" record) 0)
                                      :account-id (gethash "account-id" record)
                                      :store-path store-path
                                      :scope scope :metadata metadata))))

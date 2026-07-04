(in-package #:kli/tools/trace)

;;; The trace tool installs a reversible trace as a contribution stamped with
;;; the extension that contributed the tool, so it drains when that extension is
;;; retracted. trace-read relays the per-protocol ring the trace fills.

(defun trace-tool-extension (protocol tool)
  "The id of the extension that contributed TOOL, so a trace it installs drains
under the same owner."
  (let ((contribution (find tool (list-tool-contributions protocol)
                            :key #'contribution-tool)))
    (and contribution (contribution-extension contribution))))

(defun qualified-symbol-name (symbol)
  "SYMBOL printed PACKAGE::NAME so the resolved target is unambiguous."
  (let ((package (symbol-package symbol)))
    (if package
        (format nil "~A::~A" (package-name package) (symbol-name symbol))
        (prin1-to-string symbol))))

(defun resolve-trace-package (parameters)
  "The package an unqualified :function spec interns in: explicit :package, else
CL-USER. Signals on an unknown package."
  (let ((package (tool-parameter parameters :package)))
    (or (find-package (or package :cl-user))
        (error "Unknown package: ~A" package))))

(defun trace-contribution-for-symbol (protocol symbol)
  "An installed trace contribution already tracing SYMBOL, or nil."
  (find-if (lambda (c)
             (and (eq (contribution-kind c) :trace)
                  (eq (getf (contribution-state c) :symbol) symbol)))
           (protocol-installed-contributions protocol)))

(defun run-trace-tool (tool parameters context &key call-id on-update)
  "Trace the function named by :function (interned in :package, default
CL-USER). The trace lands as a contribution under this tool's extension, so it
reverses when the extension is retracted. Tracing a function already traced is
idempotent; a package-qualified :function whose qualifier overrides an explicit
:package flags the mismatch."
  (declare (ignore call-id on-update))
  (let* ((function-spec (or (tool-parameter parameters :function)
                            (error "Trace tool requires :function.")))
         (protocol (active-protocol context))
         (requested-package (resolve-trace-package parameters))
         (symbol (resolve-trace-symbol function-spec requested-package)))
    (if (trace-contribution-for-symbol protocol symbol)
        (make-tool-result
         :content (list (make-tool-text-content
                         (format nil "Already tracing ~A; the existing trace ~
remains in effect." (qualified-symbol-name symbol))))
         :details (list :function function-spec
                        :symbol (qualified-symbol-name symbol)
                        :package (package-name (symbol-package symbol))
                        :already-tracing-p t))
        (let* ((extension (trace-tool-extension protocol tool))
               (contribution (make-trace-contribution
                              :function-spec function-spec
                              :package (tool-parameter parameters :package)
                              :source extension
                              :extension extension)))
          (install-contribution protocol contribution context)
          (let* ((installed (getf (contribution-state contribution) :symbol))
                 (name (if installed (qualified-symbol-name installed) function-spec))
                 (mismatch-p (and (tool-parameter parameters :package)
                                  installed
                                  (not (eq (symbol-package installed)
                                           requested-package)))))
            (make-tool-result
             :content (list (make-tool-text-content
                             (format nil "Tracing ~A.~@[ ~A~] Calls accrue in the ~
session trace ring; read them with the trace-read tool."
                                     name
                                     (when mismatch-p
                                       (format nil "(The package-qualified spec ~
resolved to ~A, overriding the requested package ~A.)"
                                               (package-name (symbol-package installed))
                                               (package-name requested-package))))))
             :details (append (list :function function-spec
                                    :symbol name
                                    :package (and installed
                                                  (package-name (symbol-package installed)))
                                    :extension (and extension
                                                    (princ-to-string extension)))
                              (when mismatch-p (list :package-mismatch t)))))))))

(defun run-untrace-tool (tool parameters context &key call-id on-update)
  "Untrace the one function named by :function (interned in :package, default
CL-USER): find the matching active trace and retract just it. Errors when no
trace covers that function."
  (declare (ignore tool call-id on-update))
  (let* ((function-spec (or (tool-parameter parameters :function)
                            (error "Untrace tool requires :function.")))
         (protocol (active-protocol context))
         (symbol (resolve-trace-symbol function-spec
                                       (resolve-trace-package parameters)))
         (contribution (trace-contribution-for-symbol protocol symbol)))
    (unless contribution
      (error "No active trace for ~A." (qualified-symbol-name symbol)))
    (retract-contribution protocol contribution context)
    (make-tool-result
     :content (list (make-tool-text-content
                     (format nil "Untraced ~A." (qualified-symbol-name symbol))))
     :details (list :symbol (qualified-symbol-name symbol)
                    :package (package-name (symbol-package symbol))))))

(defun run-trace-read-tool (tool parameters context &key call-id on-update)
  "Return the session trace ring, oldest line first. When the ring has dropped
older lines past its cap, a leading marker and an :evicted count say how many."
  (declare (ignore tool parameters call-id on-update))
  (let* ((protocol (active-protocol context))
         (lines (reverse (protocol-trace-buffer protocol)))
         (evicted (protocol-trace-evicted protocol)))
    (make-tool-result
     :content (list (make-tool-text-content
                     (cond
                       ((and lines (plusp evicted))
                        (format nil "[+~D earlier trace line~:P dropped]~%~{~A~^~%~}"
                                evicted lines))
                       (lines (format nil "~{~A~^~%~}" lines))
                       (t "No trace output captured."))))
     :details (append (list :lines lines :count (length lines))
                      (when (plusp evicted) (list :evicted evicted))))))

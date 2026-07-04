(in-package #:kli/context/lens)

(defun make-context-capsule (&key messages staged-patches)
  (pandoriclet ((projected-messages (copy-list messages))
                (staged-patches (copy-list staged-patches)))
    (lambda (operation &rest arguments)
      (ecase operation
        (:inspect
         (list :messages (copy-list projected-messages)
               :staged-patches (reverse (copy-list staged-patches))))
        (:messages
         (copy-list projected-messages))
        (:set-messages
         (setf projected-messages (copy-list (first arguments))))
        (:stage
         (push (first arguments) staged-patches)
         (first arguments))
        (:staged-patches
         (reverse (copy-list staged-patches)))
        (:clear-staged
         (setf staged-patches '()))))))

(defun context-capsule-value (agent-context name)
  (get-pandoric (agent-context-capsule agent-context) name))

(defun (setf context-capsule-value) (value agent-context name)
  (setf (get-pandoric (agent-context-capsule agent-context) name) value)
  value)

(defun inspect-context (agent-context)
  (funcall (agent-context-capsule agent-context) :inspect))

(defun context-projected-messages (agent-context)
  (funcall (agent-context-capsule agent-context) :messages))

(defun context-staged-patches (agent-context)
  (funcall (agent-context-capsule agent-context) :staged-patches))

(defun set-context-projected-messages (agent-context messages)
  (funcall (agent-context-capsule agent-context) :set-messages messages)
  messages)

(defun capsule-state (capsule)
  (list :messages (copy-list (get-pandoric capsule 'projected-messages))
        :staged-patches (copy-list (get-pandoric capsule 'staged-patches))))

(defun recode-context-capsule (agent-context)
  (let ((state (capsule-state (agent-context-capsule agent-context))))
    (setf (agent-context-capsule agent-context)
          (make-context-capsule
           :messages (getf state :messages)
           :staged-patches (getf state :staged-patches))))
  agent-context)

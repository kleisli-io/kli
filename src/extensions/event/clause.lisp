(in-package #:kli/event)

(defun on-contribution-name (event-type)
  "Contribution name for the on author clause, which lowers
(on EVENT-TYPE HANDLER) to an event-handler contribution. An installed handler
only pushes into per-protocol handler storage and stays inert until the event
system dispatches its type, so it carries no requirement of its own."
  (intern (format nil "ON-~A" (normalize-extension-id event-type)) :keyword))

(defcontribution-kind :on (extension-id form)
  (destructuring-bind (_ event-type handler) form
    (declare (ignore _))
    `(make-event-handler-contribution
      :name ,(on-contribution-name event-type)
      :event-type ',(normalize-extension-id event-type)
      :handler ,handler
      :source ',extension-id)))

(register-author-clause-requirements :on '())

(in-package #:kli/author)

(defun command (builder name &key label description handler metadata)
  "Builder verb. Contribute a slash command NAME run by HANDLER. The builder
verbs are the clause heads, so command reads as data in a manifest and runs as
a function in a builder body — one word, two surfaces."
  (builder-add-contribution
   builder
   (make-command-contribution :name name
                              :label label
                              :description description
                              :runner handler
                              :metadata metadata
                              :source (extension-builder-id builder))))

(defun on (builder event-type handler)
  "Builder verb: contribute HANDLER, invoked when EVENT-TYPE is dispatched."
  (builder-add-contribution
   builder
   (make-event-handler-contribution :name (on-contribution-name event-type)
                                    :event-type (normalize-extension-id event-type)
                                    :handler handler
                                    :source (extension-builder-id builder))))

(defun notify (context text &key (level :info))
  "Surface TEXT to the user as a :notify event. A no-op when no event provider
is installed."
  (let* ((protocol (active-protocol context))
         (events (and protocol
                      (find-capability-provider protocol :events
                                                :contract :events/v1))))
    (when events
      (provider-call events :emit-event context
                     (provider-call events :make-event :notify
                                    :payload (list :text text :level level))))
    (values)))

(in-package #:kli/tools/introspect)

;;; Three read-only tools gated by :image/inspect, each requiring the
;;; introspection provider so a profile without it fails loud at install.

(defextension list-objects-tool
  (:requires
   (capability runtime/introspection :contract runtime/introspection/v1))
  (:provides
   (tool list-objects
     :label "List objects"
     :description "List the ids of the live objects in the running image. Each
id is a string you can pass to the inspect tool. Read-only."
     :parameters '(:object)
     :runner #'run-list-objects-tool
     :metadata '(:capabilities (:image/inspect)))))

(defextension context-summary-tool
  (:requires
   (capability runtime/introspection :contract runtime/introspection/v1))
  (:provides
   (tool context-summary
     :label "Context summary"
     :description "Summarize the running image: the active protocol, the control
plane, and the ids of the live objects. Read-only."
     :parameters '(:object)
     :runner #'run-context-summary-tool
     :metadata '(:capabilities (:image/inspect)))))

(defextension inspect-tool
  (:requires
   (capability runtime/introspection :contract runtime/introspection/v1))
  (:provides
   (tool inspect
     :label "Inspect"
     :description "Describe one live object in the running image by its id (as
listed by the list-objects tool): its class and, where available, its kind,
source, and version. Read-only."
     :parameters '(:object (:id :string))
     :runner #'run-inspect-tool
     :metadata '(:capabilities (:image/inspect)))))

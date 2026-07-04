(in-package #:kli/tools/trace)

(defextension trace-tool
  (:provides
   (tool trace
     :label "Trace"
     :description "Trace a function in the live image. Its calls and returns
accrue in a bounded per-session ring you read with the trace-read tool. The
trace is reversible: it is untraced when this session's extensions are torn
down. Mutates the image."
     :parameters '(:object (:function :string)
                   (:package :string :optional t))
     :runner #'run-trace-tool
     :metadata '(:capabilities (:image/eval)))))

(defextension untrace-tool
  (:provides
   (tool untrace
     :label "Untrace"
     :description "Stop tracing one function in the live image -- the reverse of
trace. :function and :package name it the same way trace does. Only that trace is
removed; others stay active. Errors when the function is not currently traced.
Mutates the image."
     :parameters '(:object (:function :string)
                   (:package :string :optional t))
     :runner #'run-untrace-tool
     :metadata '(:capabilities (:image/eval)))))

(defextension trace-read-tool
  (:provides
   (tool trace-read
     :label "Read trace output"
     :description "Return the trace-output lines captured in this session's ring,
oldest first. The ring keeps the most recent 200 lines; if older lines were
dropped, the result leads with a marker and an :evicted count. Read-only."
     :parameters '(:object)
     :runner #'run-trace-read-tool
     :metadata '(:capabilities (:image/inspect)))))

(in-package #:kli/tui/status)

(defun message-renderer-contribution (kind user-fn source)
  "An eql :method on render-transcript-event keyed by event KIND, wrapping the
   ergonomic (event theme width) -> lines USER-FN."
  (let ((k (normalize-extension-id kind)))
    (make-method-contribution
     :gf-name 'render-transcript-event
     :qualifiers '()
     :specializer-names (list (list 'eql k) t t t t)
     :lambda-list '(kind protocol event theme width)
     :body `((declare (ignore kind protocol))
             (funcall ,user-fn event theme width))
     :source source)))

(defcontribution-kind :message-renderer (extension-id form)
  (destructuring-bind (_ kind fn) form
    (declare (ignore _))
    `(message-renderer-contribution ',kind ,fn ',extension-id)))

(defclass status-slot-contribution (contribution)
  ((slot-id :initarg :slot-id :reader contribution-slot-id)
   (initial :initarg :initial :reader contribution-initial)))

(defun make-status-slot-contribution (&key name slot-id (initial "") source)
  (make-instance 'status-slot-contribution
                 :kind :status-slot :name name
                 :slot-id slot-id :initial initial :source source))

(defmethod install-contribution ((protocol extension-protocol)
                                 (c status-slot-contribution) context)
  (declare (ignore context))
  (register-status-slot protocol (contribution-slot-id c)
                        :initial (contribution-initial c))
  (push c (protocol-installed-contributions protocol))
  c)

(defmethod retract-contribution ((protocol extension-protocol)
                                 (c status-slot-contribution) context)
  (declare (ignore context))
  (unregister-status-slot protocol (contribution-slot-id c))
  (setf (protocol-installed-contributions protocol)
        (remove c (protocol-installed-contributions protocol)))
  c)

(defcontribution-kind :status-slot (extension-id form)
  (destructuring-bind (_ id &key (initial "")) form
    (declare (ignore _))
    `(make-status-slot-contribution
      :name ',(normalize-extension-id id)
      :slot-id ',(normalize-extension-id id)
      :initial ,initial
      :source ',extension-id)))

(defclass widget-contribution (contribution)
  ((widget-id :initarg :widget-id :reader contribution-widget-id)
   (widget :initarg :widget :reader contribution-widget)))

(defun make-widget-contribution (&key name widget-id widget source)
  (make-instance 'widget-contribution
                 :kind :widget :name name
                 :widget-id widget-id :widget widget :source source))

(defmethod install-contribution ((protocol extension-protocol)
                                 (c widget-contribution) context)
  (declare (ignore context))
  (register-widget protocol (contribution-widget-id c) (contribution-widget c))
  (push c (protocol-installed-contributions protocol))
  c)

(defmethod retract-contribution ((protocol extension-protocol)
                                 (c widget-contribution) context)
  (declare (ignore context))
  (unregister-widget protocol (contribution-widget-id c))
  (setf (protocol-installed-contributions protocol)
        (remove c (protocol-installed-contributions protocol)))
  c)

(defcontribution-kind :widget (extension-id form)
  (destructuring-bind (_ id factory) form
    (declare (ignore _))
    `(make-widget-contribution
      :name ',(normalize-extension-id id)
      :widget-id ',(normalize-extension-id id)
      :widget ,factory
      :source ',extension-id)))

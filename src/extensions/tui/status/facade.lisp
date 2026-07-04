(in-package #:kli/tui/status)

(defun message-renderer (builder kind fn)
  (builder-add-contribution
   builder
   (message-renderer-contribution kind fn (extension-builder-id builder))))

(defun status-slot (builder id &key (initial ""))
  (builder-add-contribution
   builder
   (make-status-slot-contribution
    :name (normalize-extension-id id)
    :slot-id (normalize-extension-id id)
    :initial initial
    :source (extension-builder-id builder))))

(defun widget (builder id factory)
  (builder-add-contribution
   builder
   (make-widget-contribution
    :name (normalize-extension-id id)
    :widget-id (normalize-extension-id id)
    :widget factory
    :source (extension-builder-id builder))))

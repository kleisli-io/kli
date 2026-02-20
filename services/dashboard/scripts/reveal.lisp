;;;; KLI Dashboard — Scroll reveal animation script

(in-package :kli-dashboard)

(defun render-reveal-script ()
  "Render the IntersectionObserver script for .reveal/.stagger animations."
  (reactive-script
    (ps:chain document (add-event-listener "DOMContentLoaded"
      (lambda ()
        (let ((observer (ps:new (-intersection-observer
                          (lambda (entries)
                            (ps:chain entries (for-each
                              (lambda (entry)
                                (when (ps:@ entry is-intersecting)
                                  (ps:chain (ps:@ entry target) class-list (add "visible"))
                                  (let ((children (ps:chain (ps:@ entry target)
                                                   (query-selector-all ".stagger"))))
                                    (dotimes (i (ps:@ children length))
                                      (let ((child (aref children i))
                                            (delay (* i 120)))
                                        (set-timeout
                                          (lambda ()
                                            (ps:chain child class-list (add "visible")))
                                          delay)))))))))
                          (ps:create :threshold 0.05)))))
          (ps:chain document (query-selector-all ".reveal")
            (for-each (lambda (el) (ps:chain observer (observe el)))))))))))

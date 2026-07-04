(in-package #:kli/tui/input)

(defextension tui-input
  (:provides
   (method decode-input () (input-decoder t) (decoder data)
     (call-behavior (input-decoder-behavior decoder) decoder data))
   (method recode-tui-behavior () (input-decoder) (decoder &rest args)
     (apply #'recode-input-decoder decoder args))))

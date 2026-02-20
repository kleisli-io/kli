;;;; KLI Dashboard — Design tokens
;;;; Matching KLI landing page aesthetic exactly.

(in-package :kli-dashboard)

(defun init-tokens ()
  "Set lol-reactive design tokens to KLI aesthetic."
  (setf lol-reactive:*colors*
    '((:background . "#08080a")
      (:surface    . "#111116")
      (:surface-2  . "#1a1a22")
      (:text       . "#c8c8d0")
      (:heading    . "#e8e8ee")
      (:muted      . "#7b7b89")
      (:accent     . "#d4a853")
      (:accent-dim . "#8a6d30")
      (:border     . "#1e1e28")
      (:primary    . "#d4a853")
      (:secondary  . "#7b7b89")
      (:success    . "#4ade80")
      (:warning    . "#d4a853")
      (:error      . "#f87171")))

  (setf lol-reactive:*typography*
    '((:family    . "'Major Mono Display', monospace")
      (:heading   . "'Space Mono', monospace")
      (:body      . "Inter, system-ui, -apple-system, sans-serif")
      (:mono      . "'Space Mono', monospace")
      (:massive   . "clamp(5rem, 18vw, 14rem)")
      (:hero      . "clamp(2.5rem, 6vw, 4.5rem)")
      (:h1        . "clamp(1.8rem, 4vw, 2.8rem)")
      (:h2        . "clamp(1.3rem, 2.5vw, 1.8rem)")
      (:body-size . "1.15rem")
      (:small     . "0.875rem")
      (:caption   . "0.75rem")))

  (setf lol-reactive:*spacing*
    '((:section . "clamp(5rem, 12vh, 10rem)")
      (:block   . "4rem")
      (:element . "2rem")
      (:tight   . "0.5rem")
      (:xs . "0.25rem")
      (:sm . "0.5rem")
      (:md . "1rem")
      (:lg . "2rem")
      (:xl . "4rem"))))

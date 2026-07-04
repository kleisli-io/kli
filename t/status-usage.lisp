(in-package #:kli/tests)

(in-suite all)

(defun status-usage-test-registry (&key (window 10000))
  "A bare registry with one selected model declaring WINDOW (NIL = none)."
  (let ((registry (models:make-model-registry)))
    (models:register-model-provider
     registry (models:make-model-provider "p" :fake) nil)
    (let ((def (models:register-model-definition
                registry
                (models:make-model-definition "p" "m" :fake :context-window window)
                nil)))
      (models:select-model registry def nil)
      registry)))

(test status-usage-percent-math
  "With a context window, tokens and percent are computed. With no window, tokens and percent are unknown and there is no readout denominator."
  (let ((r (tui-status:compute-context-usage
            :total 1000 :trailing-tokens 200 :context-window 10000)))
    (is (= 1200 (tui-status:readout-tokens r)))
    (is (= 10000 (tui-status:readout-context-window r)))
    (is (< (abs (- 12.0 (tui-status:readout-percent r))) 1d-4)))
  (let ((r (tui-status:compute-context-usage :total 1000 :context-window nil)))
    (is (null (tui-status:readout-context-window r)))
    (is (null (tui-status:readout-tokens r)))
    (is (null (tui-status:readout-percent r)))))

(test status-usage-nil-after-compaction
  "When usage is unknown the window is still reported (footer shows ?/window) but tokens and percent are nil. A compaction with no assistant after it is unknown, an assistant after the compaction restores a trustworthy count, and no compaction is never unknown."
  (let ((r (tui-status:compute-context-usage
            :total 1000 :trailing-tokens 200 :context-window 10000
            :usage-known-p nil)))
    (is (= 10000 (tui-status:readout-context-window r)))
    (is (null (tui-status:readout-tokens r)))
    (is (null (tui-status:readout-percent r))))
  (is (tui-status:usage-unknown-after-compaction-p
       (list (sess:make-message-entry (sess:make-assistant-message "old"))
             (sess:make-compaction-entry "summary" nil)
             (sess:make-message-entry (sess:make-user-message "new")))))
  (is (not (tui-status:usage-unknown-after-compaction-p
            (list (sess:make-compaction-entry "summary" nil)
                  (sess:make-message-entry (sess:make-assistant-message "fresh"))))))
  (is (not (tui-status:usage-unknown-after-compaction-p
            (list (sess:make-message-entry (sess:make-assistant-message "hi"))))))
  (is (not (tui-status:usage-unknown-after-compaction-p '()))))

(test status-usage-window-lookup-uses-current-selection
  "The window comes from the current selection. A model that declares no window, or no selection at all, yields nil."
  (is (= 10000 (tui-status:current-context-window
                (status-usage-test-registry :window 10000))))
  (is (null (tui-status:current-context-window
             (status-usage-test-registry :window nil))))
  (is (null (tui-status:current-context-window (models:make-model-registry)))))

(test status-usage-trailing-estimate-counts-after-last-assistant
  "Only the user message after the last assistant counts (here \"hello world\" estimates to 3). With no assistant message the whole list is trailing (\"abcd\" plus \"ef\" each estimate to 1)."
  (is (= 3 (tui-status:trailing-token-estimate
            (list (sess:make-user-message "first")
                  (sess:make-assistant-message "reply")
                  (sess:make-user-message "hello world")))))
  (is (= 2 (tui-status:trailing-token-estimate
            (list (sess:make-user-message "abcd")
                  (sess:make-user-message "ef"))))))

(test status-usage-session-readout-assembles-from-pieces
  "The readout assembles model, provider, totals, and cache reads from the registry and usage. Tokens add the total to the trailing estimate (120 plus 3) and the percent is that sum over the window (123 over 10000 is 1.23%)."
  (let* ((registry (status-usage-test-registry :window 10000))
         (usage (agent-session:make-context-usage
                 :input-tokens 100 :output-tokens 20
                 :cache-read-tokens 40 :cache-write-tokens 0 :total-tokens 120))
         (messages (list (sess:make-user-message "first")
                         (sess:make-assistant-message "reply")
                         (sess:make-user-message "hello world")))
         (entries (list (sess:make-message-entry (sess:make-assistant-message "reply"))))
         (r (tui-status:session-context-usage-readout
             registry (models:current-model-selection registry)
             usage messages entries)))
    (is (string= "m" (tui-status:readout-model-id r)))
    (is (string= "p" (tui-status:readout-provider r)))
    (is (= 120 (tui-status:readout-total r)))
    (is (= 40 (tui-status:readout-cache-read r)))
    (is (= 123 (tui-status:readout-tokens r)))
    (is (< (abs (- 1.23 (tui-status:readout-percent r))) 1d-4))))

(test status-usage-format-readout-renders-none-without-a-model
  "The footer line is present before a model is chosen: a readout with no model
   renders anchored by `none` rather than vanishing. A nil readout still yields
   nil, and a fully-populated readout renders the whole line."
  (is (null (tui-status:format-context-usage-readout nil)))
  (is (string= "none"
               (tui-status:format-context-usage-readout
                (tui-status:compute-context-usage))))
  (is (string= "gpt-5.5 · high · 0/272k (0%)"
               (tui-status:format-context-usage-readout
                (tui-status:compute-context-usage
                 :model-id "gpt-5.5" :thinking :high
                 :total 0 :context-window 272000)))))

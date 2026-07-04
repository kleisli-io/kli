(in-package #:kli/tests)

(test text-utilities
  (is (= 7 (text:visible-width (format nil "he~Cll" #\Tab))))
  (is (string= "he" (text:truncate-to-width "hello" 2)))
  (is (equal '("hello" "world") (text:wrap-text "hello world" 5)))
  (is (string= "hi  " (text:pad-right "hi" 4)))
  (is (equal '("a" "b" "") (text:split-lines (format nil "a~%b~%"))))
  (is (equal '("ab" "c") (text:hard-wrap "abc" 2)))
  (is (equal '("a" "b" "c") (text:split-words "a b c")))
  (is (text:blank-string-p (format nil "  ~%~C" #\Tab)))
  (is (not (text:blank-string-p "hi")))
  (is (string= "a   b" (text:normalize-text (format nil "a~Cb" #\Tab))))
  (is (text:printable-string-p "abc"))
  (is (not (text:printable-string-p (string (code-char 7)))))
  (is (= 12 (text:parse-positive-integer "12px")))
  (is (null (text:parse-positive-integer "0")))
  (is (null (text:parse-positive-integer "abc")))
  (is (text:string-prefix-p "he" "hello"))
  (is (not (text:string-prefix-p "lo" "hello")))
  (is (text:string-suffix-p "lo" "hello"))
  (is (not (text:string-suffix-p "he" "hello"))))

(test text-wrap-logical-line-respects-width
  (is (equal '("hello world")
             (text:wrap-logical-line "hello world" 20)))
  (is (equal '("hello" "world")
             (text:wrap-logical-line "hello world" 5)))
  (is (equal '("ab" "cd" "ef")
             (text:wrap-logical-line "abcdef" 2))))

(test text-jsonify-coerces-detail-plist-to-jzon-safe-shape
  "jsonify maps a structured-detail plist to the JSON shape the event log and the
model wire share: keywords downcase, nil fields drop, lists become arrays, and
booleans survive."
  (let* ((tree (text:jsonify
                (list :package "KLI-USER"
                      :condition-type "DIVISION-BY-ZERO"
                      :category :error
                      :empty nil
                      :restarts (list "ABORT" "RETRY")
                      :truncated t)))
         (record (com.inuoe.jzon:parse (com.inuoe.jzon:stringify tree))))
    (is (string= "KLI-USER" (gethash "package" record)))
    (is (string= "DIVISION-BY-ZERO" (gethash "condition-type" record)))
    (is (string= "error" (gethash "category" record))
        "keyword leaves downcase to strings")
    (is (null (nth-value 1 (gethash "empty" record)))
        "nil fields drop instead of serializing as false")
    (is (equalp #("ABORT" "RETRY") (gethash "restarts" record)))
    (is (eq t (gethash "truncated" record)))))

(test text-jsonify-hash-table-policy-selects-key-case
  "The hash-table policy is the one axis the two callers differ on: the event log
normalizes keys, while JSON-RPC arguments pass through so case-sensitive keys
survive."
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash "filePath" table) "/tmp/x"
          (gethash "missing" table) nil)
    (let ((normalized (text:jsonify table :hash-tables :normalize))
          (passed (text:jsonify table :hash-tables :passthrough)))
      (is (string= "/tmp/x" (gethash "filepath" normalized))
          "normalize downcases keys for the event log")
      (is (null (nth-value 1 (gethash "missing" normalized)))
          "normalize drops nil values")
      (is (eq table passed) "passthrough returns the very same table")
      (is (string= "/tmp/x" (gethash "filePath" passed))
          "passthrough preserves the original case-sensitive key"))))

(test text-character-width-is-wcwidth-aware
  (is (= 1 (text:character-width #\a)))
  (is (= 3 (text:character-width #\Tab)))
  (is (= 2 (text:character-width (code-char #x4E2D))) "CJK ideograph is wide")
  (is (= 2 (text:character-width (code-char #xFF21))) "fullwidth latin is wide")
  (is (= 2 (text:character-width (code-char #x1F600))) "emoji is wide")
  (is (= 0 (text:character-width (code-char #x0301)))
      "combining accent is zero width")
  (is (= 0 (text:character-width (code-char #x200D)))
      "zero width joiner is zero width")
  (is (= 0 (text:character-width (code-char #x11A8)))
      "hangul trailing jamo composes into the preceding syllable"))

(test text-width-functions-count-columns-not-characters
  (let ((cjk (format nil "~C~C" (code-char #x4E2D) (code-char #x6587)))
        (accented (format nil "e~C" (code-char #x0301))))
    (is (= 4 (text:visible-width cjk)))
    (is (= 1 (text:visible-width accented)))
    (is (string= (string (code-char #x4E2D)) (text:truncate-to-width cjk 3))
        "truncation never splits a wide character")
    (is (string= accented (text:truncate-to-width accented 1))
        "a trailing combining mark survives truncation")
    (is (= 5 (text:visible-width (text:pad-right cjk 5)))
        "padding measures columns")
    (is (equal (list cjk (string (code-char #x4E2D)))
               (text:hard-wrap (format nil "~A~C" cjk (code-char #x4E2D)) 4))
        "hard wrap slices by columns")
    (is (equal '("hello" "world") (text:hard-wrap "helloworld" 5))
        "ascii hard wrap is unchanged")
    (is (equal (list cjk)
               (text:wrap-logical-line (format nil "~A" cjk) 4))
        "a fitting wide word stays on one line")))

(test text-split-on-whitespace-breaks-on-every-whitespace-kind
  "split-on-whitespace tokenizes on space, tab, newline, and return and drops
empty fields; split-words stays space-only so wrap keeps tabs inside a word."
  (is (equal '("a" "b" "c")
             (text:split-on-whitespace (format nil "a~Cb~Cc" #\Tab #\Newline))))
  (is (equal '("a" "b")
             (text:split-on-whitespace (format nil "  a~C~C b  " #\Tab #\Return))))
  (is (null (text:split-on-whitespace (format nil " ~C~C " #\Tab #\Newline))))
  (is (equal (list (format nil "a~Cb" #\Tab) "c")
             (text:split-words (format nil "a~Cb c" #\Tab)))
      "split-words keeps the tab inside the word — the wrap path depends on this"))

(test command-extensions-share-the-canonical-whitespace-splitter
  "Both command extensions resolve split-on-whitespace to the single kli/text
definition; their former private split-words copies are gone."
  (is (eq 'text:split-on-whitespace
          (find-symbol "SPLIT-ON-WHITESPACE" '#:kli/context/commands)))
  (is (eq 'text:split-on-whitespace
          (find-symbol "SPLIT-ON-WHITESPACE" '#:kli/model/commands)))
  (is (null (find-symbol "SPLIT-WORDS" '#:kli/context/commands)))
  (is (null (find-symbol "SPLIT-WORDS" '#:kli/model/commands))))

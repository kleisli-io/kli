(in-package #:kli/tools/filesystem)

(defextension read-tool
  (:provides
   (tool read
     :label "Read"
     :description "Read a file. Anchored output rows are LINE:HH|content: HH is a 2-hex-digit hash of the line content, the first | after LINE:HH is syntax and not file content, and source content starts immediately after it, including leading spaces or later literal | characters. Edits reference these anchors. Optional start/end select an inclusive 1-based line range (end past EOF clamps). Set raw to true for unanchored content."
     :parameters '(:object (:path :string)
                   (:start :integer :optional t)
                   (:end :integer :optional t)
                   (:raw :boolean :optional t))
     :runner #'run-read-tool
     :renderer (kli/ext:make-tool-presenter
                :present-call #'kli/ext:hidden-call-presenter
                :present-result #'kli/ext:summary-result-presenter)
     :metadata '(:capabilities (:file/read)))))

(defextension find-tool
    (:provides
     (tool find
           :label "Find"
           :description "List file paths matching a glob pattern, for example src/**/*.lisp. Supports * and ** wildcards. Returns paths only, no file content."
           :parameters '(:object (:pattern :string))
           :runner #'run-find-tool
           :renderer (kli/ext:make-tool-presenter
                      :present-call #'kli/ext:hidden-call-presenter
                      :present-result #'kli/ext:find-summary-result-presenter)
           :metadata '(:capabilities (:file/read)))))

(defextension search-tool
    (:provides
     (tool search
           :label "Search"
           :description "Search file contents with a regular expression. Path is a single file or a glob, for example src/**/*.lisp. Matches print as *LINE:HH|content and context lines as space-prefixed LINE:HH|content. The leading * or space is search metadata; the first | after LINE:HH is syntax and not file content. These anchors are edit-ready - matched files need no read before editing. Optional context selects how many surrounding lines to show."
           :parameters '(:object (:pattern :string)
                                 (:path :string)
                                 (:context :integer :optional t))
           :runner #'run-search-tool
           :renderer (kli/ext:make-tool-presenter
                      :present-call #'kli/ext:hidden-call-presenter
                      :present-result #'kli/ext:search-summary-result-presenter)
           :metadata '(:capabilities (:file/read)))))

(defextension write-tool
  (:provides
   (tool write
     :label "Write"
     :description "Write a file."
     :parameters '(:object (:path :string)
                   (:content :string))
     :runner #'run-write-tool
     :renderer (kli/ext:make-tool-presenter
                :present-result #'kli/ext:diff-result-presenter)
     :metadata '(:coordinate (:atom :file/write :constraint :path-prefix :arg :path)
                 :capabilities (:file/write)))))

(defextension edit-tool
  (:provides
   (tool edit
     :label "Edit"
     :description "Apply a hashline patch. Read or search a file first. Anchored source rows arrive as LINE:HH|content; the first | after LINE:HH is syntax and not file content. Patch anchors are LINE:HH, and copied LINE:HH| anchors are tolerated.

Grammar:
@@ PATH          section header (one or more sections per patch)
+ LINE:HH        insert payload AFTER anchored line
+                insert payload at EOF (bare +)
< LINE:HH        insert payload BEFORE anchored line
<                insert payload at BOF (bare <)
- A:HH..B:HH     delete inclusive range
= A:HH..B:HH     replace range with payload (no payload = delete)
~content         payload line, everything after ~ is verbatim

Payload source starts after ~. Never include the delimiter pipe as payload unless that pipe is part of the source content after the delimiter, for example a Lisp token |symbol name| is payload ~|symbol name|.

Anchors are validated against current disk content. A stale or unread anchor rejects the whole patch (no partial apply). Common Lisp patches are validated as source syntax: a successful .lisp write must parse as valid Common Lisp source — balanced delimiters plus well-formed reader syntax, with no unterminated strings or comments and no unknown dispatch. Output that is only delimiter-unbalanced may be repaired in memory according to repair (safe by default, else reject/preview); any other source-syntax error is rejected with a bounded diagnostic and nothing is written or previewed. Under repair=safe a local, form-count-preserving repair auto-writes and is reported as an auto-repaired write; a riskier repair previews without writing and returns a candidate id (accept with accept_repair plus path only if the file has not changed). repair=reject refuses all repairs; repair=preview previews every repair, including safe ones. Non-Lisp files are written by anchors alone with no source validation. Edited files end with a trailing newline."
      :parameters '(:object (:input :string :optional t)
                    (:repair :string :optional t)
                    (:accept_repair :string :optional t)
                    (:path :string :optional t))
     :runner #'run-edit-tool
     :renderer (kli/ext:make-tool-presenter
                :present-result #'kli/ext:diff-result-presenter)
     :metadata '(:coordinate (:atom :file/edit :deriver :edit-patch-paths)
                 :capabilities (:file/edit)))))

;; The anchor cache is conversation-scoped: a session switch starts a fresh
;; conversation with none of the prior session's reads in the model's context,
;; so read-before-edit must reset. switch-agent-session emits :session-switch on
;; every path (reset, branch, rewind, resume), so clearing here covers them all.
(defextension filesystem-anchor-lifecycle
  (:requires
   (capability events :contract events/v1))
  (:provides
   (event-handler clear-anchor-cache-on-session-switch
     :event-type :session-switch
     :handler (lambda (event context)
                (declare (ignore event))
                (clrhash (anchor-cache (active-protocol context)))))))

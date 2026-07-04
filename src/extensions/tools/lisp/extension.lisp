(in-package #:kli/tools/lisp)


(defextension edit-sexp
  (:provides
   (tool edit-sexp
     :label "Edit s-expression"
     :description "Structurally replace an s-expression in Common Lisp source using a source-syntax CST. match_form matches by CST structure: list shape and reader-prefix forms must match, atoms compare by verbatim source token text, and whitespace plus line/block comments are trivia. Reader forms such as quote, function quote, feature conditionals, pathnames, complexes, arrays, radix numbers, bit vectors, and label references are matched as source forms without package lookup, readtable case folding, feature evaluation, or read-time evaluation. The match must be unique: 0 matches or >1 (without replace_all) are rejected, the latter listing every match's line:col. new_form must be exactly one source form; an empty new_form deletes the match and one adjacent separator. Inputs and final output are validated as Common Lisp source syntax, so unsafe unknown dispatch and delimiter errors are rejected before writing. Spliced continuation lines are re-indented to canonical Common Lisp columns; the splice's first line and every byte outside the match are preserved exactly. within_type/within_name scope the search to one top-level form (head atom and, for CLOS methods, name plus qualifiers/specializers, source-structurally matched). Pass dry_run true to preview the change — the diff and resulting content — without writing the file or arming anchors. A very large change shows only the (+N -M) summary in place of the diff, so re-read the file to refresh anchors. Common Lisp source only; use edit for other files."
     :parameters '(:object (:path :string)
                   (:match_form :string)
                   (:new_form :string)
                   (:within_type :string :optional t)
                   (:within_name :string :optional t)
                   (:replace_all :boolean :optional t)
                   (:dry_run :boolean :optional t))
     :runner #'run-edit-sexp-tool
     :metadata '(:coordinate (:atom :file/edit :constraint :path-prefix :arg :path)
                 :capabilities (:file/edit)))))

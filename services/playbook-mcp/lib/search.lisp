;;; Playbook MCP Server - Search Operations
;;; Keyword and semantic search for patterns

(in-package #:playbook-mcp)

;;; Keyword search
;;; DEPRECATED: Use semantic-search-patterns instead. Kept for potential internal use only.

(defun keyword-match-p (pattern keywords)
  "Check if pattern content contains all keywords (case-insensitive)."
  (let ((content-lower (string-downcase (pattern-content pattern))))
    (every (lambda (kw)
             (search (string-downcase kw) content-lower))
           keywords)))

(defun search-patterns (query &key domain (limit 20))
  "Search patterns by keyword(s).
   QUERY is a string that will be split on whitespace.
   Optional DOMAIN filters to specific domain.
   Returns list of matching patterns, sorted by effectiveness."
  (let* ((keywords (cl-ppcre:split "\\s+" query))
         (patterns (if domain
                       (patterns-by-domain domain)
                       (list-patterns)))
         (matches (remove-if-not (lambda (p) (keyword-match-p p keywords))
                                 patterns)))
    ;; Sort by effectiveness (helpful - harmful), descending
    (let ((sorted (sort (copy-list matches) #'>
                        :key #'pattern-effectiveness)))
      (if limit
          (subseq sorted 0 (min limit (length sorted)))
          sorted))))

;;; Proven patterns (high effectiveness)

(defun proven-patterns (&key domain (min-helpful 3) (limit 20))
  "Get patterns with helpful >= MIN-HELPFUL.
   These are 'proven' patterns with positive track record."
  (let* ((patterns (if domain
                       (patterns-by-domain domain)
                       (list-patterns)))
         (proven (remove-if-not (lambda (p)
                                  (>= (pattern-helpful p) min-helpful))
                                patterns)))
    (let ((sorted (sort (copy-list proven) #'>
                        :key #'pattern-effectiveness)))
      (if limit
          (subseq sorted 0 (min limit (length sorted)))
          sorted))))

;;; Harmful patterns (warnings)

(defun harmful-patterns (&key domain (min-harmful 1) (limit 20))
  "Get patterns with harmful >= MIN-HARMFUL.
   These are patterns that have caused problems."
  (let* ((patterns (if domain
                       (patterns-by-domain domain)
                       (list-patterns)))
         (harmful (remove-if-not (lambda (p)
                                   (>= (pattern-harmful p) min-harmful))
                                 patterns)))
    (let ((sorted (sort (copy-list harmful) #'>
                        :key #'pattern-harmful)))
      (if limit
          (subseq sorted 0 (min limit (length sorted)))
          sorted))))

;;; Search result formatting

(defun format-search-results (patterns)
  "Format patterns as JSON-compatible list for MCP responses."
  (mapcar #'pattern-to-json patterns))

;;; Playbook MCP Server - Status Tools
;;; Server metrics and graph health reporting (pattern operations via pq_query)

(in-package #:playbook-mcp)

;;; Status tool

(mcp-framework:define-tool playbook-status ()
  "Get playbook server status."
  (let ((edge-types (make-hash-table :test 'equal)))
    ;; Count edges by relation type
    (maphash (lambda (k edges)
               (declare (ignore k))
               (dolist (e edges)
                 (incf (gethash (string-downcase (symbol-name (edge-relation e)))
                                edge-types 0))))
             *current-graph*)
    (mcp-framework:make-text-content
     (with-output-to-string (s)
       (format s "Playbook MCP Server Status~%~%")
       (format s "Patterns loaded: ~D~%" (pattern-count))
       (format s "Embeddings cached: ~D~%" (embedding-cache-size))
       (format s "Graph: ~D nodes, ~D edges (stale: ~A)~%"
               (graph-node-count) (graph-edge-count)
               (if *graph-stale* "yes" "no"))
       (when (> (hash-table-count edge-types) 0)
         (format s "Edge types: ")
         (maphash (lambda (k v) (format s "~A=~D " k v)) edge-types)
         (format s "~%"))
       (when *last-rebuild-time*
         (format s "Last rebuild: ~A~%" (format-universal-time *last-rebuild-time*)))
       (format s "Ollama URL: ~A~%" *ollama-url*)
       (format s "Embedding model: ~A~%" *embedding-model*)))))

;;; Graph health tool

(mcp-framework:define-tool playbook-graph-health ()
  "Graph health report: orphan patterns, edge distribution, embedding coverage."
  (let* ((all-patterns (list-patterns))
         (total (length all-patterns))
         (nodes-with-edges 0)
         (orphans nil)
         (with-embeddings 0)
         (edge-counts nil))
    ;; Check each pattern
    (dolist (p all-patterns)
      (let* ((id (pattern-id p))
             (out (length (or (graph-outgoing id) nil)))
             ;; Count incoming
             (in-count 0))
        (maphash (lambda (k edges)
                   (declare (ignore k))
                   (dolist (e edges)
                     (when (string= (edge-target e) id)
                       (incf in-count))))
                 *current-graph*)
        (if (and (zerop out) (zerop in-count))
            (push id orphans)
            (incf nodes-with-edges))
        (push (+ out in-count) edge-counts)
        (when (pattern-embedding p)
          (incf with-embeddings))))
    (mcp-framework:make-text-content
     (with-output-to-string (s)
       (format s "Graph Health Report~%~%")
       (format s "Patterns: ~D total, ~D connected, ~D orphans~%"
               total nodes-with-edges (length orphans))
       (format s "Edges: ~D total across ~D nodes~%"
               (graph-edge-count) (graph-node-count))
       (format s "Embeddings: ~D/~D patterns (~D%)~%"
               with-embeddings total
               (if (> total 0) (round (* 100 (/ with-embeddings total))) 0))
       (format s "Graph stale: ~A~%" (if *graph-stale* "yes" "no"))
       (when orphans
         (format s "~%Orphan patterns (no edges): ~{~A~^, ~}~%" orphans))
       (when edge-counts
         (let ((sorted (sort edge-counts #'>)))
           (format s "~%Edge distribution: min=~D max=~D avg=~,1F~%"
                   (car (last sorted))
                   (first sorted)
                   (/ (reduce #'+ sorted) (length sorted)))))))))

;;; Utility function

(defun format-universal-time (ut)
  "Format universal time as ISO-8601 string."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time ut)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month day hour min sec)))

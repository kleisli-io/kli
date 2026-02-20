;;; Playbook MCP Server - Package Extension
;;; Adds MCP framework imports and server exports to playbook-mcp package.
;;; The base package is defined in ../lib/package.lisp.

(in-package #:playbook-mcp)

;; Import MCP framework symbols into our package for use in tools/resources/server
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(mcp-framework:make-server
            mcp-framework:run-server
            mcp-framework:define-tool
            mcp-framework:make-text-content
            mcp-framework:define-resource
            mcp-framework:content-to-json)))

;; Server exports
(export '(main start-playbook-server initialize-server))

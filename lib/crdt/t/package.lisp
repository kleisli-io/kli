(defpackage #:crdt-tests
  (:use #:cl #:crdt #:fiveam))

(in-package #:crdt-tests)

(def-suite :crdt-tests
  :description "Tests for CRDT library")

(in-suite :crdt-tests)

(defpackage :conspath.asdf
  (:use #:cl #:asdf))

(in-package :conspath.asdf)

(defsystem :conspath
  :description "Path-like matching for CL lists (etc)"
  :author "Ryan Pavlik"
  :license "BSD-2-Clause"
  :version "0.0"

  :depends-on (:alexandria :checkl)
  :pathname "src"
  :serial t

  :components
  ((:file "package")
   (:file "conspath")
   (:file "simple-matches")
   (:file "complex-matches")))

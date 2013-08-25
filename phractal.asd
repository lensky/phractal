(asdf:defsystem phractal
  :description "phractal: fast parser combinators for common lisp."
  :version "0.3"
  :author "YL"
  :depends-on (:water)
  :serial t
  :components ((:file "packages")
               (:file "combinator-conditions")
               (:file "primitive-combinators")
               (:file "combinator-utils")
               (:file "list-utils")
               (:file "pattern-matching")))

(asdf:defsystem phractal
  :description "phractal: fast parser combinators for common lisp."
  :version "0.3"
  :author "YL"
  :depends-on (:water :screamer)
  :serial t
  :components ((:file "packages")
               (:file "defcombinator")
               (:file "primitive-combinators")
               (:file "combinator-utils")
               (:file "list-utils")
               (:file "pattern-matching")))

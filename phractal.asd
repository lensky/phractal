(asdf:defsystem phractal
  :description "phractal: fast parser combinators for common lisp."
  :version "0.3"
  :author "YL"
  :depends-on (:water :iterate :let-plus :anaphora)
  :serial t
  :components ((:file "packages")
               (:file "compiler")
               (:file "combinator-types")
               (:file "base-combinators")
               (:file "combinator-utils")
               (:module "pattern-matching"
                        :serial t
                        :components ((:file "bindings")
                                     (:file "list-utils")
                                     (:file "pattern-matching")))))

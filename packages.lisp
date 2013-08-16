(defpackage #:phractal
  (:use #:cl
        #:iterate
        #:water.macro
        #:water.g/=
        #:let-plus
        #:anaphora)
  (:export :combinator
           :succeed
           :fail
           :combinator-p
           :combinator-modifier
           :combinator
           :combinator-result
           :remainder
           :state
           :combinator-success
           :value
           :combinator-failure
           :reason
           :c-failure-p
           :c-success-p)
  (:export :combinator->lisp
           :def-comb->lisp
           :compile-combinator)
  (:export :c-start
           :start-p
           :start-value
           :c-end
           :end-p
           :end-value
           :c-terminal
           :has-terminal-p
           :get-terminal
           :get-remainder
           :c-predicate
           :predicate
           :c-terminal-combinator
           :c-terminal-value
           :value
           :fn=
           :c-star
           :c-or
           :c-and
           :combinators)
  (:export :make-simple-succeed
           :*id-succeed*
           :make-simple-fail
           :*id-fail*
           :make-predicate-comb
           :make-term-val-comb
           :make-star-comb
           :make-and-comb
           :make-or-comb))

(defpackage #:phractal.pattern-matching
  (:use #:cl
        #:phractal
        #:water.macro
        #:water.g/=
        #:anaphora
        #:let-plus)
  (:export :combinator-result
           :remainder
           :state
           :combinator-success
           :value
           :combinator-failure
           :reason
           :c-failure-p
           :c-success-p)
  (:export :*id-succeed*
           :*id-fail*)
  (:export :*basic-list-start*
           :*basic-list-end*
           :*basic-list-terminal*
           :make-list-term-comb-comb
           :make-list-term-val-comb
           :make-list-comb
           :make-sublist-comb)
  (:export :make-literal-var
           :make-atomic-single-var
           :make-listing-var
           :make-list-pat
           :make-sublist-pat))

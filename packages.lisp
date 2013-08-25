(defpackage #:phractal
  (:use #:cl
        #:water.macro)
  (:export :combinator-failure
           :combiantor-succeess
           :value
           :remainder
           :state
           :defcombinator
           :if-c-success
           :try-again
           :c-start
           :c-end
           :c-terminal
           :c-predicate
           :c-subcombinator
           :c-star
           :c-and
           :c-or
           :c-plus
           :with-id-success
           :all-values
           :first-value))

(defpackage #:phractal.list-utils
  (:use #:cl
        #:phractal)
  (:export :c-list-start
           :c-list-end
           :c-list-terminal
           :c-sublist
           :c-list))

(defpackage #:phractal.pattern-matching
  (:use #:cl
        #:phractal
        #:phractal.list-utils
        #:water.g/=)
  (:export :make-literal-var
           :make-atomic-single-var
           :make-listing-var
           :make-list-pat
           :make-sublist-pat))

(defpackage #:phractal-package
  (:use #:cl
        #:screamer))

(in-package #:phractal-package)

(define-screamer-package #:phractal
  (:use #:cl
        #:water.macro
        #:screamer)
  (:export :value
           :remainder
           :state
           :call-combinator
           :defcombinator
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

(define-screamer-package #:phractal.list-utils
  (:use #:cl
        #:phractal
        #:screamer)
  (:export :c-list-start
           :c-list-end
           :c-list-terminal
           :c-sublist
           :c-list))

(define-screamer-package #:phractal.pattern-matching
  (:use #:cl
        #:phractal
        #:phractal.list-utils
        #:water.g/=
        #:screamer)
  (:export :make-literal-var
           :make-atomic-single-var
           :make-listing-var
           :make-list-pat
           :make-sublist-pat))

(in-package #:phractal)

(defun make-simple-succeed (v &rest f-of-v)
  `((,v r s)
    (declare (ignore r s))
    ,@f-of-v))

(defparameter *id-succeed* (make-simple-succeed 'v 'v))

(defun make-simple-fail (r &rest f-of-r)
  `((,r s)
    (declare (ignore s))
    ,@f-of-r))

(defparameter *id-fail* (make-simple-fail 'r 'r))

;; Constructors

(defun make-predicate-comb (predicate combinator succeed fail &key (base-class 'c-predicate))
  (make-instance base-class
                 :predicate predicate
                 :combinator combinator
                 :succeed succeed
                 :fail fail))

(defun make-term-val-comb (value succeed fail &key (fn= #'g/=) (base-class 'c-terminal-value))
  (make-instance base-class
                 :value value
                 :fn= fn=
                 :succeed succeed
                 :fail fail))

(defun make-star-comb (combinator succeed fail &key (base-class 'c-star))
  (make-instance base-class
                 :combinator combinator
                 :succeed succeed
                 :fail fail))

(defun make-and-comb (combinators succeed fail &key (base-class 'c-and))
  (make-instance base-class
                 :combinators combinators
                 :succeed succeed
                 :fail fail))

(defun make-or-comb (combinators succeed fail &key (base-class 'c-or))
  (make-instance base-class
                 :combinators combinators
                 :succeed succeed
                 :fail fail))

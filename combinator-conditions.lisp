(in-package #:phractal)

(define-condition combinator-result () ()
  (:documentation "Base condition for combinator results."))

(define-condition combinator-failure (combinator-result)
  ((reason :reader reason
           :initarg :reason
           :initform "Combinator failure."))
  (:documentation "Base condition for combinator failures."))

(define-condition combinator-success (combinator-result)
  ((value :reader value
          :initarg :value
          :initform nil)
   (remainder :reader remainder
              :initarg :remainder
              :initform (error "Must provide a remainder to a condition of combinator-success."))
   (state :reader state
          :initarg :state
          :initform (error "Must provide a state to a condition of combinator-success.")))
  (:documentation "Base condition for combinator successes."))

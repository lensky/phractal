(in-package #:phractal)

(defclass combinator ()
  ((succeed :reader succeed
            :initarg :succeed
            :initform (error "Must provide success function.")
            :documentation "Function of match, remainder, and state.")
   (fail :reader fail
         :initarg :fail
         :initform (error "Must provide failure function.")
         :documentation "Function of input or failure and state."))
  (:documentation "Generic class for any combinator."))

(defun combinator-p (x)
  (subtypep (type-of x) 'combinator))

(defclass combinator-modifier (combinator)
  ((combinator :reader combinator
               :initarg :combinator
               :initform (error "Must provide combinator.")))
  (:documentation "Generic class for combinator modifiers."))

(defclass combinator-result ()
  ((remainder :reader remainder
              :initarg :remainder
              :initform (error "Must provide a remainder."))
   (state :reader state
          :initarg :state
          :initform (error "Must provide a state."))))

(defclass combinator-success (combinator-result)
  ((value :reader value
          :initarg :value
          :initform (error "Must provide a value.")))
  (:documentation "Class to be returned from combinators after a successful match."))

(defmethod print-object ((combinator-success combinator-success) stream)
  (print-unreadable-object (combinator-success stream :type t)
    (with-slots (value remainder state) combinator-success
     (format stream ":value ~a :remainder ~a :state ~a" value remainder state))))

(defclass combinator-failure (combinator-result)
  ((reason :reader reason
           :initarg :reason
           :initform (error "Must provide a failure reason")))
  (:documentation "Class to be returned from combinators after a failed match."))

(defmethod print-object ((combinator-failure combinator-failure) stream)
  (print-unreadable-object (combinator-failure stream :type t)
    (with-slots (reason remainder state) combinator-failure
      (format stream ":reason ~a :remainder ~a :state ~a" reason remainder state))))

(defun c-failure-p (x)
  (subtypep (type-of x) 'combinator-failure))
(defun c-success-p (x)
  (subtypep (type-of x) 'combinator-success))

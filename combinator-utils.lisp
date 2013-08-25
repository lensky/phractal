(in-package #:phractal)

(defun with-id-success (pre-combinator)
  (funcall pre-combinator #'values))

(defun all-values (combinator input state)
  (let ((values '())
        (states '()))
   (handler-bind
       ((combinator-success
         (lambda (s)
           (setf values (cons (value s) values))
           (setf states (cons (state s) states))))
        (combinator-failure #'signal))
     (funcall combinator input state))
   (values (nreverse values) (nreverse states))))

(defun first-value (combinator input state)
  (handler-case (funcall combinator input state)
    (combinator-success (success)
      (values (value success) (state success)))))

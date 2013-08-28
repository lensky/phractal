(in-package #:phractal)

(defun with-id-success (pre-combinator)
  (funcall pre-combinator #'values))

(defmacro first-value (combinator input state)
  `(screamer:one-value
    (screamer:either
      (funcall-nondeterministic ,combinator ,input ,state)
      '())))

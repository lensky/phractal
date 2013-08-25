(in-package #:phractal.list-utils)

(defparameter c-list-start (c-start #'listp))
(defparameter c-list-end (c-end #'null))
(defparameter c-list-terminal
  (c-terminal (lambda (x) (and (listp x) (not (null x))))
            #'car #'cdr))

(defun c-sublist (combinator &key (recursive-name (gensym)))
  (c-subcombinator combinator (funcall c-list-terminal #'values) :recursive-name recursive-name))

(defun c-list (combinators)
  (c-and
   (cons (with-id-success c-list-start)
         (append 
          combinators
          (list (with-id-success c-list-end))))))

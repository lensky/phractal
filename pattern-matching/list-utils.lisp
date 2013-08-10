(in-package #:phractal.pattern-matching)

(defparameter *basic-list-start*
  (make-instance 'c-start
                 :start-p '((v)
                            `(and (listp ,v) (not (null ,v))))
                 :start-value :list-start
                 :succeed *id-succeed*
                 :fail (make-simple-fail
                        'r
                        '`(format nil "Not at start of list: ~a." ,r))))

(defparameter *basic-list-end*
  (make-instance 'c-end
                 :end-p #'null
                 :end-value :list-end
                 :succeed *id-succeed*
                 :fail (make-simple-fail
                        'r
                        '`(format nil "Not at end of list: ~a." ,r))))

(defclass c-list-terminal (c-terminal)
  ((has-terminal-p :reader has-terminal-p
                   :initform '((v)
                               `(and (listp ,v) (not (null ,v)))))
   (get-terminal :reader get-terminal
                 :initform #'car)
   (get-remainder :reader get-remainder
                  :initform #'cdr)))

(defparameter *basic-list-terminal*
  (make-instance 'c-list-terminal :succeed *id-succeed*
                 :fail (make-simple-fail
                        'r
                        '`(format nil "No list terminal: ~a." ,r))))

(defclass c-list-terminal-value (c-terminal-value c-list-terminal) ())

(defclass c-list-terminal-combinator (c-terminal-combinator c-list-terminal) ())

(defun make-list-term-comb-comb (combinator succeed fail)
  (make-instance 'c-list-terminal-combinator
                 :combinator combinator
                 :succeed succeed
                 :fail fail))

(defun make-list-term-val-comb (value succeed fail
                                &key
                                  (base-class 'c-list-terminal-value)
                                  (fn= #'g/=))
  (make-term-val-comb value succeed fail :base-class base-class :fn= fn=))

(defun make-list-comb (combinators succeed fail)
  (make-and-comb (cons *basic-list-start*
                       (append combinators (list *basic-list-end*)))
                 succeed
                 fail))

(defclass c-sublist (combinator)
  ((combinators :reader combinators
                :initarg :combinators
                :initform (error "Must provide combinators."))))

(def-comb->lisp (c-sublist input state succeed fail
                           :value-slots (combinators)
                           :vars (c-res c-gen))
    (let ((c-compiled
           (compile-combinator
            (make-list-comb combinators *id-succeed* *id-fail*))))
      `(let ((,c-gen (funcall ,c-compiled (car ,input) ,state)))
         (lambda ()
           (let ((,c-res (funcall ,c-gen)))
             (if (c-success-p ,c-res)
                 (,succeed (value ,c-res) (cdr ,input) (state ,c-res))
                 (,fail ,c-res ,state)))))))

(defun make-sublist-comb (combinators succeed fail)
  (make-instance 'c-sublist
                 :combinators combinators
                 :succeed succeed
                 :fail fail))

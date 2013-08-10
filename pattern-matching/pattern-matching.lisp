(in-package #:phractal.pattern-matching)

(defclass c-alist-binding-var (c-binding-var)
  ((set-binding :reader set-binding
                :initform (lambda (name state value)
                            (let ((binding (assoc name state)))
                              (if binding
                                  (let ((nstate (copy-alist state)))
                                    (setf (cdr (assoc name nstate)) (cons value (cdr binding)))
                                    nstate)
                                  (cons (cons name (list value)) state))))
                :allocation :class)))

(defclass c-alist-single-binding-var (c-single-binding-var)
  ((set-binding :reader set-binding
                :initform (lambda (name state value)
                            (let ((binding (assoc name state)))
                              (if binding
                                  (let ((nstate (copy-alist state)))
                                    (setf (cdr (assoc name nstate)) value)
                                    nstate)
                                  (cons (cons name value) state))))
                :allocation :class)
   (get-binding :reader get-binding
                :initform (lambda (name state)
                            (assoc name state))
                :allocation :class)
   (binding-value :reader binding-value
                  :initform #'cdr
                  :allocation :class)))

(defun make-binding-comb (name combinator
                          &key
                            (fail
                             (make-simple-fail
                              'r ``(format nil "Failed to bind ~a in ~a." ',',name ,r)))
                            (base-class 'c-alist-binding-var))
  (make-instance base-class
                 :name (or name (gensym))
                 :combinator combinator
                 :fail fail))

(defun make-single-binding-comb (name combinator
                                 &key
                                   (fail
                                    (make-simple-fail
                                     'r ``(format nil "Failed to bind ~a in ~a." ',',name ,r)))
                                   (fn= #'g/=)
                                   (base-class 'c-alist-single-binding-var))
  (make-instance base-class
                 :name (or name (gensym))
                 :combinator combinator
                 :fail fail
                 :fn= fn=))

;; Pattern matching API

(defun make-literal-var (value &optional (fn= #'g/=))
  (make-list-term-val-comb value *id-succeed* *id-fail* :fn= fn=))

(defun make-atomic-single-var (name)
  (make-single-binding-comb name *basic-list-terminal*))

(defun make-listing-var (name)
  (make-binding-comb name (make-star-comb *basic-list-terminal*
                                          *id-succeed*
                                          *id-fail*)))

(defun make-list-pat (&rest combinators)
  (make-list-comb combinators *id-succeed* *id-fail*))

(defun make-sublist-pat (&rest combinators)
  (make-sublist-comb combinators *id-succeed* *id-fail*))

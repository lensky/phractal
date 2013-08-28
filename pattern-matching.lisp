(in-package #:phractal.pattern-matching)

(defcombinator c-bindingvar (name combinator set-binding)
    (input state succeed fail)
  (declare (type function set-binding)
           (type symbol name))
  (let ((result (call-combinator combinator input state)))
    (succeed (value result) (remainder result) (funcall set-binding name state (value result)))))

(defcombinator c-single-bindingvar (name combinator set-binding get-binding binding-value eq-predicate)
    (input state succeed fail)
  (declare (type function set-binding get-binding binding-value eq-predicate)
           (type symbol name))
  (let ((result (call-combinator combinator input state))
        (binding (funcall get-binding name state)))
    (if binding
        (let ((binding-value (funcall binding-value binding)))
          (if (funcall eq-predicate (value result) binding-value)
              (succeed (value result) (remainder result) (state result))
              (fail)))
        (succeed (value result) (remainder result) (funcall set-binding name state (value result))))))

(defun c-alist-bindingvar (name combinator)
  (c-bindingvar
   name combinator
   (lambda (name state value)
     (let ((binding (assoc name state)))
       (if binding
           (let ((nstate (copy-alist state)))
             (setf (cdr (assoc name nstate)) (cons value (cdr binding)))
             nstate)
           (cons (cons name (list value)) state))))))

(defun c-alist-single-bindingvar (name combinator eq-predicate)
  (c-single-bindingvar
   name combinator
   (lambda (name state value)
     (let ((binding (assoc name state)))
       (if binding
           (let ((nstate (copy-alist state)))
             (setf (cdr (assoc name nstate)) value)
             nstate)
           (cons (cons name value) state))))
   #'assoc
   #'cdr
   eq-predicate))

(defun make-literal-var (value &optional (eq-predicate #'g/=))
  (with-id-success
      (c-predicate (lambda (x) (funcall eq-predicate value x)) (with-id-success c-list-terminal))))

(defun make-atomic-single-var (name &optional (eq-predicate #'g/=))
  (with-id-success
      (c-alist-single-bindingvar name (with-id-success c-list-terminal) eq-predicate)))

(defun make-listing-var (name)
  (with-id-success
      (c-alist-bindingvar name (with-id-success (c-star (with-id-success c-list-terminal))))))

(defun make-list-pat (&rest combinators)
  (with-id-success
      (c-list combinators)))

(defun make-sublist-pat (&rest combinators)
  (with-id-success
      (c-sublist
       (with-id-success (apply #'c-list combinators)))))

(in-package #:phractal)

(defcombinator c-start (start-p)
    (input state succeed fail)
  (declare (type function start-p))
  (if (funcall start-p input)
      (succeed nil input state)
      (fail)))

(defcombinator c-end
    (end-p)
    (input state succeed fail)
  (declare (type function end-p))
  (if (funcall end-p input)
      (succeed nil input state)
      (fail)))

(defcombinator c-terminal (has-terminal-p get-terminal get-remainder)
    (input state succeed fail)
  (declare (type function has-terminal-p get-terminal get-remainder))
  (if (funcall has-terminal-p input)
      (succeed (funcall get-terminal input) (funcall get-remainder input) state)
      (fail)))

(defcombinator c-subcombinator (sub-combinator combinator)
    (input state succeed fail)
  (let ((parent-result (call-combinator combinator input state)))
    (let ((sub-result (call-combinator sub-combinator (value parent-result) (state parent-result))))
      (succeed (value sub-result) (remainder parent-result) (state sub-result)))))

(defcombinator c-predicate (pred-fn combinator)
    (input state succeed fail)
  (declare (type function pred-fn))
  (let ((result (call-combinator combinator input state)))
    (if (funcall pred-fn (value result))
        (succeed (value result) (remainder result) (state result))
        (fail))))

(defmacro nondeterministic-flet ((&body fn-specs) &body body)
  (labels ((parse-spec (fn-spec)
             (destructuring-bind (name args &body body) fn-spec
               `(,name (lambda (,name ,@args) ,@body))))
           (rebind-spec (fn-spec)
             (destructuring-bind (name args &body body) fn-spec
               (declare (ignore body))
               `(,name (lambda ,args
                         (funcall-nondeterministic ,name ,name ,@args))))))
    `(let (,@(mapcar #'parse-spec fn-specs))
       (let (,@(mapcar #'rebind-spec fn-specs))
         ,@body))))

(defcombinator c-star (combinator)
    (input state succeed fail)
  (nondeterministic-flet
      ((helper
        (values remainder state)
        (let ((result (call-combinator combinator remainder state)))
          (let ((nvalues (nconc values (list (value result))))
                (nremainder (remainder result))
                (nstate (state result)))
            (either (succeed nvalues nremainder nstate)
                    (funcall-nondeterministic helper helper nvalues nremainder nstate))))))
    (either
      (succeed '() input state)
      (funcall-nondeterministic helper '() input state))))

(defcombinator c-and (combinators)
    (input state succeed fail)
  (nondeterministic-flet
      ((helper (combinators values remainder state)
               (if (null combinators)
                   (succeed values remainder state)
                   (let ((result (call-combinator (car combinators) remainder state)))
                     (let ((nvalues (append values (list (value result))))
                           (nremainder (remainder result))
                           (nstate (state result)))
                       (funcall-nondeterministic helper helper (cdr combinators) nvalues nremainder nstate))))))
    (funcall-nondeterministic helper combinators '() input state)))

(defcombinator c-or (combinators)
    (input state succeed fail)
  (nondeterministic-flet
      ((helper (combinators)
               (if (null combinators)
                   (fail)
                   (either
                     (let ((result (call-combinator (car combinators) input state)))
                       (succeed (value result) (remainder result) (state result)))
                     (funcall-nondeterministic helper helper (cdr combinators))))))
    (funcall-nondeterministic helper combinators)))

(defun c-plus (combinator)
  (c-and (list combinator (funcall (c-star combinator) #'values))))

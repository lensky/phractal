(in-package #:phractal)

(defun format-symbol (format symbol)
  (intern (string-upcase (format nil format (symbol-name symbol)))))

(defparameter *tmp-recursive-combinators* '())

(defmacro defcombinator (name
                         (&rest init-arg-spec)
                            (input state succeed fail)
                         &body body)
  (with-gensyms (succeed-fn new-value new-state fn-name)
    `(defun ,name (,@init-arg-spec &key (recursive-name (gensym)))
       (declare (optimize (speed 3) (debug 0)))
       (lambda (,succeed-fn)
         (declare (type function ,succeed-fn))
         (macrolet ((,succeed (match remainder &optional (nstate ',state))
                      `(restart-case
                           (multiple-value-bind (,',new-value ,',new-state) (funcall ,',succeed-fn ,match ,nstate)
                             (signal 'combinator-success
                                     :value ,',new-value
                                     :state ,',new-state
                                     :remainder ,remainder))
                         (try-again () nil)))
                    (,fail ()
                      `(signal 'combinator-failure)))
           (labels ((,fn-name (,input ,state)
                      (declare (optimize (speed 3) (debug 0)))
                      (let ((*tmp-recursive-combinators* (cons (cons recursive-name #',fn-name)
                                                               *tmp-recursive-combinators*)))
                        ,@body
                        (,fail))))
             #',fn-name))))))

(defmacro if-c-success ((combinator input state) result &body body)
  `(handler-bind
       ((combinator-success
         (lambda (,result)
           ,(car body)
           (try-again)))
        (combinator-failure
         (lambda (,result)
           ,(cadr body))))
     (funcall (if (symbolp ,combinator)
                  (cdr (assoc ,combinator *tmp-recursive-combinators*))
                  ,combinator)
              ,input ,state)))

(defun try-again () (invoke-restart 'try-again))

(defcombinator c-start (start-p)
    (input state succeed fail)
  (declare (type function start-p))
  (if (funcall start-p input)
      (succeed nil input)
      (fail)))

(defcombinator c-end
    (end-p)
    (input start succeed fail)
  (declare (type function end-p))
  (if (funcall end-p input)
      (succeed nil input)
      (fail)))

(defcombinator c-terminal (has-terminal-p get-terminal get-remainder)
    (input state succeed fail)
  (declare (type function has-terminal-p get-terminal get-remainder))
  (if (funcall has-terminal-p input)
      (succeed (funcall get-terminal input) (funcall get-remainder input))
      (fail)))

(defcombinator c-subcombinator (sub-combinator combinator)
    (input state succeed fail)
  (declare (type function sub-combinator combinator))
  (if-c-success (combinator input state) result
    (if-c-success (sub-combinator (value result) (state result)) sub-result
      (succeed (value sub-result) (remainder result) (state sub-result))
      (signal sub-result))
    (signal result)))

(defcombinator c-predicate (pred-fn combinator)
    (input state succeed fail)
  (declare (type function pred-fn combinator))
  (if-c-success (combinator input state) result
    (when (funcall pred-fn (value result))
      (succeed (value result) (remainder result) (state result)))
    (signal result)))

(defcombinator c-star (combinator)
    (input state succeed fail)
  (declare (type function combinator))
  (succeed '() input)
  (labels ((helper (results remainder state)
             (if-c-success (combinator remainder state) result
               (let ((nresults (append results (list (value result))))
                     (nremainder (remainder result))
                     (nstate (state result)))
                 (succeed nresults nremainder nstate)
                 (helper nresults nremainder nstate))
               (signal result))))
    (helper '() input state)))

(defcombinator c-and (combinators)
    (input state succeed fail)
  (labels ((helper (combinators results remainder state)
             (if (null combinators)
                 (succeed results remainder state)
                 (if-c-success ((car combinators) remainder state) result
                   (let ((nresults (append results (list (value result))))
                         (nremainder (remainder result))
                         (nstate (state result)))
                     (helper (cdr combinators) nresults nremainder nstate))
                   (signal result)))))
    (helper combinators '() input state)))

(defcombinator c-or (combinators)
    (input state succeed fail)
  (labels ((helper (combinators)
             (unless (null combinators)
               (if-c-success ((car combinators) input state) result
                 (succeed (value result) (remainder result) (state result))
                 '())
               (helper (cdr combinators)))))
    (helper combinators)))

(defun c-plus (combinator)
  (declare (type function combinator))
  (c-and (list combinator (funcall (c-star combinator) #'values))))

(in-package #:phractal)

(defclass combinator-success ()
  ((value :reader value
          :initarg :value)
   (state :reader state
          :initarg :state)
   (remainder :reader remainder
              :initarg :remainder)))

(defparameter *tmp-recursive-combinators* '())

(declaim (inline call-combinator))
(defun call-combinator (combinator input state)
  (funcall-nondeterministic (if (symbolp combinator)
                                (cdr (assoc combinator *tmp-recursive-combinators*))
                                combinator)
                            input state))

(defmacro defcombinator (name
                         (&rest init-arg-spec)
                            (input state succeed fail)
                         &body body)
  (with-gensyms (succeed-fn succeed-tmp fn-name)
    `(flet ((,succeed-tmp (fn match remainder nstate)
              (multiple-value-bind (new-value new-state) (funcall fn match nstate)
                (make-instance 'combinator-success
                               :value new-value
                               :state new-state
                               :remainder remainder)))
            (,fail () (fail)))
       (macrolet ((,succeed (match remainder state)
                    `(funcall #',',succeed-tmp ,',succeed-fn ,match ,remainder ,state)))
         (defun ,name (,@init-arg-spec &key (recursive-name nil))
           (declare (optimize (speed 3) (debug 0)))
           (lambda (,succeed-fn)
             (declare (type function ,succeed-fn))
             (let ((,fn-name
                    (lambda (,fn-name ,input ,state)
                      (declare (optimize (speed 3) (debug 0)))
                      ,@body)))
               (let ((,fn-name
                      (lambda (,input ,state)
                        (funcall-nondeterministic ,fn-name ,fn-name ,input ,state))))
                 (if recursive-name
                     (lambda (,input ,state)
                       (let ((*tmp-recursive-combinators*
                              (cons (cons recursive-name ,fn-name)
                                    *tmp-recursive-combinators*)))
                         (funcall-nondeterministic ,fn-name ,input ,state)))
                     ,fn-name)))))))))

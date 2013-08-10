(in-package #:phractal)

(defgeneric combinator->lisp (combinator input state)
  (:documentation "Implement this for a combinator to compile it to lisp code."))

(defmacro def-comb->lisp ((comb input state succeed fail
                                &key fn-slots value-slots vars wrap-style c-result)
                          &body body)
  (let ((macrolet-specs
         (iter (for slot in (append fn-slots (list succeed fail)))
               (for slot-value = `(slot-value ,comb ',slot))
               (collect (list slot (gensym)
                              (eval-once (slot-value)
                                `(cond
                                   ((combinator-p ,slot-value)
                                    (list '(input state)
                                          `(combinator->lisp ,,slot-value input state)))
                                   ((functionp ,slot-value)
                                    (list '(&rest args)
                                          ``(funcall ,,,slot-value ,@args)))
                                   (t ,slot-value))))))))
    (with-gensyms (tmp-succeed tmp-fail res-value res-remainder res-state c-gen)
      `(defmethod combinator->lisp ((,comb ,comb) input state)
         (let (,@(iter (for slot in value-slots)
                       (collect `(,slot (slot-value ,comb ',slot))))
               ,@(iter (for (slot-name sym macro-gen) in macrolet-specs)
                       (nconcing (list `(,slot-name ',sym) `(,sym ,macro-gen)))))
           (let ((,input input)
                 (,state state)
                 (,tmp-succeed succeed)
                 (,tmp-fail fail)
                 (,succeed ',(gensym))
                 (,fail ',(gensym)))
             (with-gensyms ,(append vars (if c-result (list c-result)))
               `(macrolet (,,@(iter (for (nil sym nil) in macrolet-specs)
                                    (collect ``(,',sym ,@,sym))))
                  (macrolet ((,succeed (,',res-value ,',res-remainder ,',res-state)
                               `(make-instance 'combinator-success
                                               :value (,',,tmp-succeed
                                                       ,,',res-value ,,',res-remainder ,,',res-state)
                                               :remainder ,,',res-remainder
                                               :state ,,',res-state))
                             (,fail (,',res-remainder ,',res-state)
                               `(make-instance 'combinator-failure
                                               :reason (,',,tmp-fail
                                                        ,,',res-remainder ,,',res-state)
                                               :remainder ,,',res-remainder
                                               :state ,,',res-state)))
                    ,,@(case wrap-style
                             (function
                              `(`(let ((run-once nil)
                                       (value (progn
                                                ,,@body)))
                                   (lambda ()
                                     (if run-once
                                         (make-instance 'combinator-failure
                                                        :reason "Extra runs of combinator fn."
                                                        :state ,,state
                                                        :remainder ,,input)
                                         (progn
                                           (setf run-once t)
                                           value))))))
                             (combinator
                              `(`(let ((,',c-gen (,combinator ,input ,state)))
                                   (lambda ()
                                     (let ((,,c-result (funcall ,',c-gen)))
                                       (if (c-success-p ,,c-result)
                                           ,,(car body)
                                           ,,(cadr body)))))))
                             (otherwise `,body)))))))))))

(defun compile-combinator (combinator)
  (compile nil `(lambda (input state) ,(combinator->lisp combinator 'input 'state))))

(in-package #:phractal.pattern-matching)

(defclass c-binding-var (combinator-modifier)
  ((name :reader name
         :initarg :name
         :initform (gensym)
         :documentation "Name of variable to get from bindings. Can be blank, in which case will be random.")
   (succeed :reader succeed
            :initform *id-succeed*)
   (set-binding :reader set-binding
                :initarg :set-binding
                :initform (error "Must providing binding setter."))))

(def-comb->lisp (c-binding-var input state succeed fail
                               :fn-slots (combinator set-binding)
                               :value-slots (name)
                               :wrap-style combinator
                               :c-result c-res)
  `(,succeed (value ,c-res) (remainder ,c-res)
             (,set-binding ',name (state ,c-res) (value ,c-res)))
  `(,fail ,c-res ,state))

(defclass c-single-binding-var (c-binding-var)
  ((fn= :reader fn=
        :initarg :fn=
        :initform (error "Must provide binding comparator."))
   (binding-value :reader binding-value
                   :initarg :binding-value
                   :initform '((b) b))
   (get-binding :reader get-binding
                :initarg :get-binding
                :initform (error "Must provide binding getter."))))

(def-comb->lisp (c-single-binding-var input state succeed fail
                                      :fn-slots (combinator set-binding binding-value fn= get-binding)
                                      :value-slots (name)
                                      :vars (binding-val)
                                      :wrap-style combinator
                                      :c-result c-res)
  `(aif (,get-binding ',name ,state)
        (let ((,binding-val (,binding-value it)))
          (if (,fn= ,binding-val
                    (value ,c-res))
              (,succeed ,binding-val (remainder ,c-res) ,state)
              (,fail ,c-res ,state)))
        (,succeed (value ,c-res) (remainder ,c-res)
                  (,set-binding ',name (state ,c-res) (value ,c-res))))
  `(,fail ,c-res ,state))

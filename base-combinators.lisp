(in-package #:phractal)

(defclass c-start (combinator)
  ((start-p :reader start-p
            :initarg :start-p
            :initform (error "Must supply start predicate."))
   (start-value :reader start-value
                :initarg :start-value
                :initform nil))
  (:documentation "Combinator to match start of input."))

(def-comb->lisp (c-start input state succeed fail
                         :fn-slots (start-p)
                         :value-slots (start-value)
                         :wrap-style function)
  `(if (,start-p ,input)
       (,succeed ',start-value ,input ,state)
       (,fail ,input ,state)))

(defclass c-end (combinator)
  ((end-p :reader end-p
          :initarg :end-p
          :initform (error "Must supply end predicate."))
   (end-value :reader end-value
              :initarg :end-value
              :initform nil))
  (:documentation "Combinator to match end of input."))

(def-comb->lisp (c-end input state succeed fail
                       :fn-slots (end-p)
                       :value-slots (end-value)
                       :wrap-style function)
  `(if (,end-p ,input)
       (,succeed ',end-value ,input ,state)
       (,fail ,input ,state)))

(defclass c-terminal (combinator)
  ((has-terminal-p :reader has-terminal-p
                   :initarg :has-terminal-p
                   :initform (error "Must supply terminal predicate."))
   (get-terminal :reader get-terminal
                 :initarg :get-terminal
                 :initform (error "Must supply terminal getter."))
   (get-remainder :reader get-remainder
                  :initarg :get-remainder
                  :initform (error "Must supply a remainder getter.")))
  (:documentation "Combinator to match any terminal node of an input."))

(def-comb->lisp (c-terminal input state succeed fail
                            :fn-slots (has-terminal-p get-terminal get-remainder)
                            :wrap-style function)
    `(if (,has-terminal-p ,input)
         (,succeed (,get-terminal ,input) (,get-remainder ,input) ,state)
         (,fail ,input ,state)))

(defclass c-predicate (combinator-modifier)
  ((predicate :reader predicate
              :initarg :predicate
              :initform (error "Must supply predicate."))))

(def-comb->lisp (c-predicate input state succeed fail
                             :fn-slots (combinator predicate)
                             :wrap-style combinator
                             :c-result c-res)
  `(if (,predicate (value ,c-res))
       (,succeed (value ,c-res) (remainder ,c-res) (state ,c-res))
       (,fail ,c-res ,input))
  `(,fail ,c-res ,input))

(defclass c-terminal-value (c-terminal)
  ((value :reader value
          :initarg :value
          :initform (error "Must supply a value."))
   (fn= :reader fn=
        :initarg :fn=
        :initform #'g/=))
  (:documentation "Combinator to match specific terminal nodes of an input."))

(def-comb->lisp (c-terminal-value input state succeed fail
                                  :fn-slots (has-terminal-p
                                             get-terminal
                                             get-remainder
                                             fn=)
                                  :vars (t-val)
                                  :value-slots (value)
                                  :wrap-style function)
    `(if (,has-terminal-p ,input)
         (let ((,t-val (,get-terminal ,input)))
           (if (,fn= ,t-val ',value)
               (,succeed ',value (,get-remainder ,input) ,state)
               (,fail ,input ,state)))
         (,fail ,input ,state)))

(defclass c-terminal-combinator (c-terminal combinator-modifier) ())

(def-comb->lisp (c-terminal-combinator input state succeed fail
                                       :fn-slots (get-remainder combinator get-terminal has-terminal-p)
                                       :vars (has-term c-res c-gen))
  `(let ((,has-term (,has-terminal-p ,input)))
     (let ((,c-gen (and ,has-term (,combinator (,get-terminal ,input) ,state))))
       (lambda ()
         (if (not ,has-term)
             (,fail ,input ,state)
             (let ((,c-res (funcall ,c-gen)))
               (if (c-success-p ,c-res)
                   (,succeed (value ,c-res) (,get-remainder ,input) (state ,c-res))
                   (,fail ,c-res ,state))))))))

(defclass c-star (combinator-modifier) ()
  (:documentation "Combinator to match zero or more repeated instances of another combinator."))

;; Note the DFS structure
(def-comb->lisp (c-star input state succeed fail
                        :fn-slots (combinator)
                        :vars (nodes c-gen remainder values c-res helper failed-once))
  `(let ((,failed-once nil)
         (,nodes (list (list (,combinator ,input ,state) '()))))
     (labels ((,helper ()
                (let+ (((,c-gen ,values) (car ,nodes)))
                  (let ((,c-res (funcall ,c-gen)))
                    (if (c-success-p ,c-res)
                        (let ((,remainder (remainder ,c-res))
                              (,values (append ,values (list (value ,c-res))))
                              (,state (state ,c-res)))
                          (setf ,nodes (cons (list (,combinator ,remainder ,state)
                                                   ,values)
                                             ,nodes))
                          (,succeed ,values ,remainder ,state))
                        (if (null (cdr ,nodes))
                            (if ,failed-once
                                (,fail ,input ,state)
                                (progn
                                  (setf ,failed-once t)
                                  (,succeed '() ,input ,state)))
                            (progn
                              (setf ,nodes (cdr ,nodes))
                              (,helper))))))))
       #',helper)))

(defclass c-or (combinator)
  ((combinators :reader combinators
                :initarg :combinators
                :initform (error "Must supply combinators."))
   (recursive-combinators :reader recursive-combinators
                          :initarg :recursive-combinators
                          :initform '())))

(def-comb->lisp (c-or input state succeed fail
                      :value-slots (combinators recursive-combinators)
                      :vars (prev-fail helper c-gen c-res remaining-cs reached-recursive))
  (let ((compiled (mapcar
                   #'compile-combinator
                   combinators)))
    `(let ((,c-gen nil)
           (,remaining-cs (list ,@compiled))
           (,reached-recursive nil))
       (labels ((,helper (,prev-fail)
                  (if ,prev-fail
                      (if ,reached-recursive
                          (if (null ,remaining-cs)
                              (,fail ,input ,state)
                              (progn
                                (setf ,c-gen (funcall (compile-combinator (car ,remaining-cs))
                                                      ,input ,state))
                                (setf ,remaining-cs (cdr ,remaining-cs))
                                (,helper nil)))
                          (if (null ,remaining-cs)
                              (progn
                                (setf ,remaining-cs (list ,@recursive-combinators))
                                (setf ,reached-recursive t)
                                (,helper t))
                              (progn
                                (setf ,c-gen (funcall (car ,remaining-cs) ,input ,state))
                                (setf ,remaining-cs (cdr ,remaining-cs))
                                (,helper nil))))
                      (let ((,c-res (funcall ,c-gen)))
                        (if (c-success-p ,c-res)
                            (,succeed (value ,c-res) (remainder ,c-res) (state ,c-res))
                            (,helper t))))))
         (lambda () (,helper t))))))

(defclass c-and (combinator)
  ((combinators :reader combinators
                :initarg :combinators
                :initform (error "Must supply combinators."))))

(def-comb->lisp (c-and input state succeed fail
                       :value-slots (combinators)
                       :vars (c-res c-gen values remainder restcombs helper nodes))
  (let ((compiled-combs (mapcar #'compile-combinator combinators)))
    `(let ((,nodes (list (list (funcall ,(car compiled-combs) ,input ,state)
                               '()
                               (list ,@(cdr compiled-combs))))))
       (labels ((,helper ()
                  (let+ (((,c-gen ,values ,restcombs) (car ,nodes)))
                    (let ((,c-res (funcall ,c-gen)))
                      (if (c-success-p ,c-res)
                          (let ((,remainder (remainder ,c-res))
                                (,values (append ,values (list (value ,c-res))))
                                (,state (state ,c-res)))
                            (if (null ,restcombs)
                                (,succeed ,values ,remainder ,state)
                                (progn
                                  (setf ,nodes (cons (list (funcall (car ,restcombs) ,remainder ,state)
                                                           ,values
                                                           (cdr ,restcombs))
                                                     ,nodes))
                                  (,helper))))
                          (if (null (cdr ,nodes))
                              (,fail ,c-res ,state)
                              (progn
                                (setf ,nodes (cdr ,nodes))
                                (,helper))))))))
         #',helper))))

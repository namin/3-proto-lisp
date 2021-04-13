;;; structural-field.lsp

(in-package 3-proto-lisp)

;; Structural field

;; Classes implementing the ProtoLisp types.
;; Instances of these classes are the internal representation of ProtoLisp values.
;; Instances of these classes are called internal structures.

(defclass handle ()
((cl-value :initarg :cl-value :accessor cl-value)))

;; NUMBER

(defclass numeral (handle)
  ())

;; BOOLEAN

(defclass boolean (handle)
  ())

;; CLOSURE

(defclass closure (handle)
  ((body :initarg :body :accessor body :initform nil)
   (lexical-envrionment :initarg :lexical-environment :accessor lexical-environment :initform nil)
   (name :initarg :name :accessor name :initform nil) ; for debugging
   (argument-pattern :initarg :argument-pattern :accessor argument-pattern :initform nil)))

(defclass primitive-closure (closure)
  ())

(defclass reflective-closure (closure)
  ())

;; Abnormal closures are primitive closures whose arguments are not all normalized

(defclass abnormal-closure (primitive-closure)
  ())

;; RAIL

(defclass rail (handle)
  ())

;; Helper class, for representing rails in CL

(defclass wrapped-cl-list ()
  ((cl-list :initarg :cl-list :accessor cl-list)))

;; ATOM

(defclass atom (handle)
  ())

(defclass pair (handle)
  ())

;; Operations boolean

(defmethod boolean-p ((boolean boolean))
  T)

(defmethod boolean-p (smth)
  nil)

;; Operations atom

(defmethod atom-p ((atom atom))
  T)

(defmethod atom-p (smth)
  nil)

;; Operations numeral

(defmethod numeral-p ((numeral numeral))
  T)

(defmethod numeral-p (smth)
  nil)

;; Operations pair

(defmethod pair-p ((pair pair))
  T)

(defmethod pair-p (smth)
  nil)

;; Operations closure

(defmethod primitive-p ((primitive-closure primitive-closure))
  T)

(defmethod primitive-p (smth)
  nil)

(defmethod reflective-p ((reflective-closure reflective-closure))
  T)

(defmethod reflective-p (smth)
  nil)

(defmethod abnormal-p ((abnormal-closure abnormal-closure))
  T)

(defmethod abnormal-p (smth)
  nil)

(defmethod closure-p ((closure closure))
  T)

(defmethod closure-p (smth)
  nil)

;; Operations pair

(defmethod pcar ((pair pair))
  (wrap (car (unwrap pair))))

(defmethod pcdr ((pair pair))
  ;; returns a rail
  (wrap (make-instance 'wrapped-cl-list :cl-list (cdr (unwrap pair)))))

(defmethod pcons ((handle1 handle) (handle2 handle))
  (wrap (cons (unwrap handle1) (unwrap handle2))))

;; Operations rail

(defmethod rail-p ((rail rail))
  T)

(defmethod rail-p (smth)
  nil)

(defmethod rcons (&rest list-of-structures)
  (wrap (make-instance 'wrapped-cl-list :cl-list (mapcar (function unwrap) list-of-structures))))

(defmethod scons (&rest list-of-structures)
  (make-instance 'wrapped-cl-list :cl-list list-of-structures))

(defmethod pcons ((handle handle) (rail rail))
  (wrap (cons (unwrap handle) (cl-list (unwrap rail)))))

(defmethod prep ((handle handle) (rail rail))
  (wrap (make-instance 'wrapped-cl-list :cl-list (cons (unwrap handle) (cl-list (unwrap rail))))))

(defmethod prep (smth (wrapped-cl-list wrapped-cl-list))
  (make-instance 'wrapped-cl-list :cl-list (cons smth (cl-list wrapped-cl-list))))

(defmethod length ((rail rail))
  (cl:length (cl-list (unwrap rail))))

(defmethod nth (nr (rail rail))
  (wrap (cl:nth nr (cl-list (unwrap rail)))))

(defmethod nth (nr (wrapped-cl-list wrapped-cl-list))
  (cl:nth nr (cl-list wrapped-cl-list)))

(defmethod pcar ((rail rail))
  (nth 0 rail))

(defmethod pcdr ((rail rail))
  (tail 1 rail))

(defmethod tail (nr (rail rail))
  (wrap (make-instance 'wrapped-cl-list :cl-list (cl:nthcdr nr (cl-list (unwrap rail))))))

(defmethod tail (nr (wrapped-cl-list wrapped-cl-list))
  (make-instance 'wrapped-cl-list :cl-list (cl:nthcdr nr (cl-list wrapped-cl-list))))

(defmethod first ((rail rail))
  (wrap (cl:first (cl-list (unwrap rail)))))

(defmethod rest ((rail rail))
  (wrap (make-instance 'wrapped-cl-list :cl-list (cl:rest (cl-list (unwrap rail))))))

(defmethod rest ((wrapped-cl-list wrapped-cl-list))
  (make-instance 'wrapped-cl-list :cl-list (cl:rest (cl-list wrapped-cl-list))))

(defmethod empty-p ((rail rail))
  (null (cl-list (unwrap rail))))

(defmethod empty-p ((wrapped-cl-list wrapped-cl-list))
  (null (cl-list wrapped-cl-list)))

;; Equality

(defmethod proto-lisp= ((handle1 handle) (handle2 handle))
  (proto-lisp= (unwrap handle1) (unwrap handle2)))

(defmethod proto-lisp= ((rail1 rail) (rail2 rail))
  (declare (special *cl-false*))
  *cl-false*)

(defmethod proto-lisp= ((handle1 handle) smth)
  (declare (special *cl-false*))
  *cl-false*)

(defmethod proto-lisp= (smth (handle1 handle))
  (declare (special *cl-false*))
  *cl-false*)

(defmethod proto-lisp= (smth1 smth2)
  (declare (special *cl-true* *cl-false*))
  (if (equal smth1 smth2)
      *cl-true*
      *cl-false*))

;; Internalization & Parsing

;; Reads from the standard input, these are plain CL values, which are mapped onto ProtoLisp values.
;; Most syntax of ProtoLisp overlaps with CL's syntax, for other cases there is a read macro.

(defun three-lisp-read-and-parse ()
  (let ((input (read)))
    (internalize input)))

;; Helper classes because syntax per type is unique in ProtoLisp, but not in CL.

(defclass cl-boolean ()
  ())

(defparameter *cl-true* (make-instance 'cl-boolean))
(defparameter *cl-false* (make-instance 'cl-boolean))

(defmethod cl-bool ((obj (eql *cl-true*)))
  T)

(defmethod cl-bool ((obj (eql *cl-false*)))
  nil)

(defmethod cl-bool (smth)
  (error "cl-bool: ~s is not either *cl-true* or cl-false" smth))

(defun cl->cl-bool (smth)
  (if smth
      *cl-true*
      *cl-false*))

(defmethod proto-lisp= ((smth1 wrapped-cl-list) (smth2 wrapped-cl-list))
  (if (equal (cl-list smth1) (cl-list smth2))
      *cl-true*
      *cl-false*))

;; sequence equivalent

(defmethod length ((wrapped-cl-list wrapped-cl-list))
  (cl:length (cl-list wrapped-cl-list)))

;; Wrapping and Unwrapping

(defclass cl-closure ()
  ((3l-closure :initarg :closure :reader closure))
  (:metaclass funcallable-standard-class))

(defmethod cl-closure-p ((cl-closure cl-closure))
  T)

(defmethod cl-closure-p (smth)
  nil)

(defmethod wrap ((cl-closure cl-closure))
  (closure cl-closure))

(defmethod wrap (smth)
  (internalize smth))

(defmethod wrap ((function function))
  (make-instance 'primitive-closure :cl-value function))

(defmethod unwrap ((handle handle))
  (cl-value handle))

(defmethod unwrap ((primitive-closure primitive-closure))
  (cl-value primitive-closure))

(defmethod unwrap ((reflective-closure reflective-closure))
  (let ((cl-closure (make-instance 'cl-closure :closure reflective-closure)))
    (set-funcallable-instance-function
     cl-closure
     (lambda (&rest args)
       (declare (ignore args))
       (error "Don't call reflective closures within Common Lisp code.")))
    cl-closure))

(defmethod unwrap ((closure closure))
  (declare (special *global*))
  (let ((cl-closure (make-instance 'cl-closure :closure closure)))
    (set-funcallable-instance-function
     cl-closure
     (lambda (args)
       (reduce closure
               (make-instance 'rail
                              :cl-value (make-instance 'wrapped-cl-list :cl-list (list args)))
               *global*
               (create-meta-continuation))))
    cl-closure))

;;; internalization.lsp

(in-package 3-proto-lisp)

;; Added syntax

;; Rails

(set-macro-character
 #\] #'(lambda (stream char)
         (declare (ignore char))
         (read stream t nil t) nil))

(set-macro-character
#\[ #'(lambda (stream char)
        (declare (ignore char))
        (make-instance 'wrapped-cl-list :cl-list (read-delimited-list #\] stream t))))

;; Booleans

(defmethod identify-cl-boolean ((symbol (eql 'T)))
  *cl-true*)

(defmethod identify-cl-boolean ((symbol (eql 'F)))
  *cl-false*)

(defmethod identify-cl-boolean (smth)
  (error "Error while parsing ~s, $ is reserved syntax for booleans." smth))

(set-macro-character
#\$ #'(lambda (stream char)
        (declare (ignore char))
        (identify-cl-boolean (read stream t nil t))))

;; Handle (cf ')
;; In order not to confuse Common Lisp, we use ^ instead of ' here.
(set-macro-character
 #\^ #'(lambda (stream char)
         (declare (ignore char))
         (internalize (read stream t nil t))))

;; Internalize wraps CL values to internal structures
(defmethod internalize ((handle handle))
  (make-instance 'handle :cl-value handle))

(defmethod internalize ((handle string))
  (make-instance 'handle :cl-value handle))

(defmethod internalize ((number number))
  (make-instance 'numeral :cl-value number))

(defmethod internalize ((cl-boolean cl-boolean))
  (make-instance 'boolean :cl-value cl-boolean))

(defmethod internalize ((wrapped-cl-list wrapped-cl-list))
  (make-instance 'rail :cl-value (make-instance 'wrapped-cl-list
                                                :cl-list (cl-list wrapped-cl-list))))

(defmethod internalize ((symbol symbol))
  (make-instance 'atom :cl-value symbol))

(defmethod internalize ((cons cons))
  (make-instance 'pair :cl-value cons))

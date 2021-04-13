;;; externalization.lsp

(in-package 3-proto-lisp)

;; Externalization

(defmethod external-type ((numeral numeral))
  'numeral)

(defmethod external-type ((number number))
  'number)

(defmethod external-type ((cl-boolean cl-boolean))
  'truth-value)

(defmethod external-type ((closure closure))
  'closure)

(defmethod external-type ((function function))
  'function)

(defmethod external-type ((rail rail))
  'rail)

(defmethod external-type ((wrapped-cl-list wrapped-cl-list))
  'sequence)

(defmethod external-type ((atom atom))
  'atom)

(defmethod external-type ((symbol symbol))
  'symbol)

(defmethod external-type ((pair pair))
  'pair)

(defmethod external-type ((cons cons))
  'procedure-call)

(defmethod external-type ((handle handle))
  'handle)

(defmethod print-to-string ((handle handle))
  (if (eql (class-of handle) (find-class 'handle))
      (format nil "?~a" (print-to-string (cl-value handle)))
      (format nil "~s" (cl-value handle))))

(defmethod print-to-string ((boolean boolean))
  (cond ((eql (unwrap boolean) *cl-true*) "$T")
        ((eql (unwrap boolean) *cl-false*) "$F")
        (t (error "print-to-string: Trying to print erronous boolean."))))

(defmethod print-to-string ((wrapped-cl-list wrapped-cl-list))
  (format nil "~s" (cl-list wrapped-cl-list)))

(defmethod print-to-string ((rail rail))
  (with-output-to-string (s)
    (format s "[ ")
    (loop for handle in (cl-list (unwrap rail))
          do (format s (print-to-string handle)))
    (format s " ]")))

(defmethod print-to-string (smth)
  (format nil "~a" smth))

(defmethod print-to-string ((primitive-closure primitive-closure))
  "<primitive procedure>")

(defmethod print-to-string ((reflective-closure reflective-closure))
  "<reflective procedure>")

(defmethod print-to-string ((closure closure))
  "<simple procedure>")

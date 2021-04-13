;;; normalization.lsp

(in-package 3-proto-lisp)

;; Normalization

;; Environments

(defclass environment (handle)
  ((bindings :initarg :bindings :initform '() :accessor bindings)))

(defmethod environment-p ((environment environment))
  T)

(defmethod environment-p (smth)
  nil)

(defmethod ccons ((atom atom) (environment environment) (rail rail) pair)
  (case (unwrap atom)
    (simple
     (make-instance 'closure
                     :name 'anonymous :lexical-environment environment :argument-pattern rail :body pair))
    (reflect
     (make-instance 'reflective-closure
                     :name 'anonymous :lexical-environment environment :argument-pattern rail :body pair))
    (t
     (error "ccons: unknown procedure type: ~s" (unwrap atom)))))

(defmethod print-to-string ((environment environment))
  "<environment>")

(defmethod make-mapping ((atom atom) value)
  (list atom value 'mapping))

(defmethod mapping-value (mapping)
  (if (and (listp mapping) (eql (car (last mapping)) 'mapping))
      (second mapping)
      (error "Mapping expected.")))

(defmethod set-binding ((environment environment) (atom atom) (handle handle))
  (push (make-mapping atom handle) (bindings environment)))

(defmethod add-binding ((environment environment) (atom atom) (handle handle))
  (make-instance 'environment
                 :bindings (cons (make-mapping atom handle) (bindings environment))))

(defmethod bind ((environment environment) (argument-pattern rail) (arguments rail))
  (cond ((empty-p argument-pattern) environment)
        ((= (length argument-pattern) (length arguments))
         (bind (add-binding environment (first argument-pattern) (first arguments))
               (rest argument-pattern) (rest arguments)))
        (t
         (error "bind: Function called with the wrong number of arguments."))))

(defmethod binding ((atom atom) (environment environment))
  (let ((mapping (find (unwrap atom) (bindings environment)
                       :key (lambda (atom+value) (unwrap (car atom+value))))))
    (if (null mapping)
        (error "binding: variable ~s unbound in env." (unwrap atom))
        (mapping-value mapping))))

;; Global environment
(defparameter *global* (make-instance 'environment))

;; Normalize

(defun normalize (internal-structure environment continuation)
  (cond ((normal-p internal-structure)
         (funcall continuation internal-structure))
        ((atom-p internal-structure)
         (funcall continuation (binding internal-structure environment)))
        ((rail-p internal-structure)
         (normalize-rail internal-structure environment continuation))
        ((pair-p internal-structure)
         (reduce (pcar internal-structure) (pcdr internal-structure)
                 environment continuation))
        (t
         (error "normalize: Error trying to normalize non internal structure ~s"
                internal-structure))))

(defun normal-p (internal-structure)
  (if (rail-p internal-structure)
      (normal-rail-p internal-structure)
      (and (not (atom-p internal-structure))
           (not (pair-p internal-structure)))))

(defmethod normal-rail-p ((rail rail))
  (or (empty-p rail)
      (and (normal-p (first rail))
           (normal-rail-p (rest rail)))))

(defun normalize-rail (rail environment continuation)
  (if (empty-p rail)
      (funcall continuation (rcons))
      (normalize (first rail)
                 environment
                 (lambda (first!)
                   (normalize-rail (rest rail)
                                   environment
                                   (lambda (rest!)
                                     (funcall continuation (prep first! rest!))))))))

(defun reduce (procedure arguments environment continuation)
  (normalize procedure
             environment
             (lambda (procedure!)
               (cond ((reflective-p procedure!)
                      (reduce-reflective procedure! arguments environment continuation))
                     ((abnormal-p procedure!)
                      (reduce-abnormal procedure! arguments environment continuation))
                     (t
                      (normalize arguments
                                 environment
                                 (lambda (arguments!)
                                   (if (primitive-p procedure!)
                                       (funcall continuation
                                                (wrap (apply (unwrap procedure!)
                                                             (cl-list (unwrap arguments!)))))
                                       (normalize (body procedure!)
                                                  (bind (lexical-environment procedure!)
                                                        (argument-pattern procedure!) arguments!)
                                                  continuation)))))))))

(defun reduce-reflective (procedure! arguments environment continuation)
  (let ((non-reflective-closure (de-reflect procedure!)))
    (normalize (body non-reflective-closure)
               (bind (lexical-environment non-reflective-closure)
                     (argument-pattern non-reflective-closure)
                     (wrap (make-instance 'wrapped-cl-list
                                          :cl-list (list environment continuation arguments))))
               (create-meta-continuation))))

(defun reduce-abnormal (procedure! arguments environment continuation)
  (ecase (name procedure!)
    (set (reduce-set arguments environment continuation))
    (lambda (reduce-lambda 'closure arguments environment continuation))
    (lambda-reflect (reduce-lambda 'reflective-closure arguments environment continuation))
    (if (reduce-if arguments environment continuation))
    (apply (reduce-apply arguments environment continuation))
    (apply-abnormal (reduce-apply-abnormal arguments environment continuation))))

(defun reduce-set (arguments environment continuation)
  (declare (special *global*))
  (let ((atom (first arguments)) ;; do not normalize the symbol
        (expression (first (rest arguments)))) ;; normalize the expression
    (normalize expression environment
               (lambda (expression!)
                 (set-binding *global* atom expression!)
                 (funcall continuation (wrap 'ok))))))

(defun reduce-if (arguments environment continuation)
  (let ((condition (first arguments))
        (consequent (first (rest arguments)))
        (antesequent (first (rest (rest arguments)))))
    (normalize condition environment
               (lambda (condition!)
                 (if (cl-bool (unwrap condition!)) ;; Not Lisp-style bools
                     (normalize consequent
                                environment
                                (lambda (consequent!)
                                  (funcall continuation consequent!)))
                     (normalize antesequent
                                environment
                                (lambda (antesequent!)
                                  (funcall continuation antesequent!))))))))

(defun reduce-lambda (closure-class-name arguments environment continuation)
  (let ((argument-pattern (first arguments))
        (body (first (rest arguments))))
    (funcall continuation
             (make-instance closure-class-name
                            :name 'anonymous
                            :body body
                            :argument-pattern
                            (wrap (make-instance 'wrapped-cl-list
                                                 :cl-list (unwrap argument-pattern)))
                            :lexical-environment environment))))

(defmethod de-reflect ((reflective-closure reflective-closure))
  (make-instance 'closure
                 :name (name reflective-closure)
                 :argument-pattern (argument-pattern reflective-closure)
                 :body (body reflective-closure)
                 :lexical-environment (lexical-environment reflective-closure)))

(defun reduce-apply (arguments environment continuation) ;; apply takes a symbol and a rail
  (normalize arguments environment
             (lambda (arguments!)
               (reduce (first arguments!) (unwrap (first (rest arguments!)))
                       environment continuation))))

(defun reduce-apply-abnormal (arguments environment continuation)
  (normalize arguments environment
             (lambda (arguments!)
               (reduce (first arguments!)
                       (unwrap (first (rest (rest arguments!))))
                       (unwrap (first (rest arguments!)))
                       continuation))))



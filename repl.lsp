;;; repl.lsp
(in-package 3-proto-lisp)
(defun read-normalize-print ()
  (declare (special *global*))
  (normalize (prompt&read)
             *global*
             (lambda (result!)
               (prompt&reply result!)
               (read-normalize-print))))

(defun prompt&read ()
  (format t ">")
  (three-lisp-read-and-parse))

(defun prompt&reply (result)
  (format t "~%~a~&" (print-to-string result)))

;; Normalize from string

;; Flag to see if in repl mode
(defparameter *repl-mode* T)

(defun normalize-from-string (string)
  (let ((*repl-mode* nil))
    (normalize (internalize (read-from-string string)) *global* (lambda (result!) result!))))

(defun create-meta-continuation ()
  (if *repl-mode*
      (lambda (result!)
        (prompt&reply result!)
        (read-normalize-print))
      (function unwrap)))

;; Loading ProtoLisp code from a file

(defun load-proto-lisp-file (filename-as-string)
  (declare (special *global*))
  (let ((path (make-pathname :name filename-as-string)))
    (with-open-file (str path :direction :input)
      (loop for line = (read str nil ’eof)
            until (eql line ’eof)
            do (normalize (internalize line) *global* (lambda (result!) result!))))))

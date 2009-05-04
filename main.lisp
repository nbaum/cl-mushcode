
(load "util")
(load "eval")
(load "function")

(format t "mushcode interpreter 1.0~%")
(finish-output)
(loop
 (format t "> ")
 (finish-output)
 (handler-case
  (handler-bind
   ((warning (lambda (w)
               (format t "warning: ~A~%" w))))
   (let ((line (read-line)))
     (eval-argument (make-string-input-stream line) *standard-output*)
     (terpri)
     (finish-output)))
  (end-of-file (c)
               (declare (ignore c))
               (return))
  (condition (c)
             (format t "error: ~A~%" c)
             (finish-output)))))
;
;(princ (eval-expression "[list-functions()]"))


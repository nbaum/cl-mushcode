
(in-package :lisp-mush)

(defun handle-connection (client)
  (with-open-socket (client)
    (let* ((stream (socket-make-stream client :input t :output t :external-format :latin-1))
           (telnet-io (make-instance 'telnet-stream :stream stream))
           (*standard-input* telnet-io)
           (*standard-output* telnet-io))
      (format t "lisp-mush 1.0~%")
      (finish-output)
      (loop
       (format t "> ")
       (finish-output)
       (handler-case
        (handler-bind
         ((warning (lambda (w)
                     (format telnet-io "warning: ~A~%" w))))
         (let ((line (read-line)))
           (eval-argument (make-string-input-stream line) *standard-output*)
           (terpri)
           (finish-output)))
         (end-of-file (c)
           (declare (ignore c))
           (return))
         (condition (c)
           (format telnet-io "error: ~A~%" c)
           (finish-output (telnet-stream telnet-io)))
         )))))

(defun listen-loop (server)
  (let* ((client (sb-bsd-sockets:socket-accept server)))
    (handle-connection client))
  (listen-loop server))

(defun main ()
  (with-server (server :port 1701)
    (format t "Listening...~%")
    (listen-loop server)))

(eval-when (:execute)
  (main))

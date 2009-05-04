
(declaim (ftype function
                eval-function
                eval-expression))

(defvar *eval-function-required* nil)

(defmacro define-evaluator (name lambda-list &body forms)
  `(defun ,name (input &optional output &rest args)
     (flet ((,name ,lambda-list ,@forms))
       (cond
        ((and (stringp input) (null output))
         (with-output-to-string (output)
           (with-input-from-string (input input)
             (apply #',name input output args))))
        ((stringp input)
         (with-input-from-string (input input)
           (apply #',name input output args)))
        ((null output)
         (with-output-to-string (output)
           (apply #',name input output args)))
        (t
         (apply #',name input output args))))))

(define-evaluator eval-argument (input output &optional terms)
  (let ((*eval-function-required* nil))
    (eval-function input output terms)))

(define-evaluator scan-argument (input output &optional terms)
  (loop
    (let ((c (read-char input nil nil)))
      (when (member c (cons nil terms))
        (return-from scan-argument c))
      (when c
        (write-char c output))
      (case c
        (#\[
         (let ((c (scan-argument input output '(#\]))))
           (when c
             (write-char c output))))
        (#\{
         (let ((c (scan-argument input output '(#\}))))
           (when c
             (write-char c output))))
        (#\(
         (let ((c (scan-argument input output '(#\)))))
           (when c
             (write-char c output))))
        ((#\\ #\%)
         (let ((c (read-char input nil nil)))
           (when c
             (write-char c output))))))))

(define-evaluator eval-function (input output &optional terms)
  (let* ((string-stream (make-string-output-stream))
         (char (eval-expression input string-stream (cons #\( terms)))
         (function (get-output-stream-string string-stream)))
    (unless (eq char #\()
      ;; Not a real function.
      (write-string function output)
      (return-from eval-function char))
    (let* ((arguments nil)
           (fsym (find-symbol (string-upcase function) :lmf)))
      (unless (fboundp fsym)
        (when *eval-function-required*
          (return-from eval-function
            (prog1
              (eval-expression input (make-broadcast-stream) terms)
              (format output "#-1 FUNCTION (~:@(~A~)) NOT FOUND" function))))
        (write-string function output)
        (write-char char output)
        (return-from eval-function (eval-expression input output terms)))
      (loop
       (peek-char t input)
        (setf char
              (if (get fsym :noeval)
                  (scan-argument input string-stream '(#\, #\)))
                (eval-argument input string-stream '(#\, #\)))))
        (push (get-output-stream-string string-stream) arguments)
        (unless (eq char #\,)
          ;; Last argument.
          (return)))
      ;; Reverse the argument list.
      (setf arguments (nreverse arguments))
      (unless (or (cdr arguments)
                  (< 0 (length (car arguments))))
        (setf arguments nil))
      (let ((*standard-output* output))
        (apply fsym arguments))
      ;; Evaluate the postscript.
      (eval-expression input output terms)
      )))

(define-evaluator eval-substitution (input output &optional terms)
  (declare (ignore terms))
  (let ((c (read-char input nil nil)))
    (case c
      ((nil))
      ((#\B #\b)
       (write-char #\Space output))
      ((#\R #\r)
       (write-char #\Newline output))
      ((#\( #\) #\{ #\} #\[ #\] #\,)
       (write-char c output))
      ((#\i)
       (let ((c (read-char input nil nil)))
         (write-string (elt *itext-stack* (position c "0123456789")) output)))
      (t
       (warn "deprecation: undefined substitution %~A is treated like \\~@*~A" c)
       (write-char c output))
      )))

(define-evaluator eval-expression (input output &optional terms)
  (loop
    (let ((c (read-char input nil nil)))
      (when (member c (cons nil terms))
        (return-from eval-expression c))
      (case c
        (#\[
         (let ((*eval-function-required* t))
           (eval-function input output '(#\]))))
        (#\{
         (eval-expression input output '(#\})))
        #|(#\(
        (eval-expression input output '(#\))))|#
        (#\\
         (let ((c (read-char input nil nil)))
           (when c
             (write-char c output))))
        (#\%
         (eval-substitution input output nil))
        (t
         (write-char c output))))))

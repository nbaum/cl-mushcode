
(require :parse-number)
(require :split-sequence)
(require :cl-ppcre)
(import 'parse-number:parse-number)
(import 'split-sequence:split-sequence)
(import 'cl-ppcre:register-groups-bind)

(defpackage lisp-mush-function
  (:nicknames :lmf)
  (:use))

(defmacro define-mush-function (name lambda-list &body forms)
  (let ((oname name)
        (name (intern (symbol-name name) :lmf))
        (lambda-list (if (listp lambda-list)
                         lambda-list
                       (car forms)))
        (flag (if (listp lambda-list)
                  nil
                lambda-list))
        (forms (if (listp lambda-list)
                  forms
                (cdr forms))))
    `(progn
       (defun ,name ,lambda-list
         ,@forms)
       ,@(when flag
           `((setf (get ',name ',flag) t)))
       ',name)))

(define-mush-function @@ :noeval (&rest args)
  (declare (ignore args)))

(define-mush-function abs (number)
  (write (abs (parse-number number))))

(define-mush-function accent (string template)
  (loop :for s :across string
        :for x :across template
        :do
        (case x
          (#\` (write-char (or (ignore-errors (elt "¿»Ã“Ÿ‡ËÏÚ˘" (position s "AEIOUaeiou"))) s)))
          (#\' (write-char (or (ignore-errors (elt "¡…Õ”⁄›·ÈÌÛ˙˝" (position s "AEIOUYaeiouy"))) s)))
          (#\~ (write-char (or (ignore-errors (elt "√—’„Òı" (position s "ANOano"))) s)))
          (#\^ (write-char (or (ignore-errors (elt "¬ Œ‘€‚ÍÓÙ˚" (position s "AEIOUaeiou"))) s)))
          (#\: (write-char (or (ignore-errors (elt "ƒÀœ÷‹‰ÎÔˆ¸ˇ" (position s "AEIOUaeiouy"))) s)))
          (#\o (write-char (or (ignore-errors (elt "≈Â" (position s "Aa"))) s)))
          (#\, (write-char (or (ignore-errors (elt "«Á" (position s "Cc"))) s)))
          (#\u (write-char (or (ignore-errors (elt "ø°" (position s "?!"))) s)))
          (#\" (write-char (or (ignore-errors (elt "´ª" (position s "<>"))) s)))
          (#\B (write-char (or (ignore-errors (elt "ﬂ" (position s "s"))) s)))
          (#\| (write-char (or (ignore-errors (elt "ﬁ˛" (position s "Pp"))) s)))
          (#\- (write-char (or (ignore-errors (elt "–" (position s "D"))) s)))
          (#\& (write-char (or (ignore-errors (elt "" (position s "o"))) s)))
          (t (write-char s)))))

(define-mush-function accname (object)
  (declare (ignore object))
  (error "ACCNAME not implemented"))

(define-mush-function acos (cosine &optional angle-type)
  (declare (ignore angle-type))
  (setf cosine (parse-number cosine))
  (write (acos cosine)))

(define-mush-function add (&rest args)
  (write (reduce #'+ args :key #'parse-number)))

(define-mush-function after (string1 string2)
  (ignore-errors (write-string (subseq string1 (1+ (search string2 string1))))))

(define-mush-function alias (player &optional new-alias)
  (declare (ignore player new-alias))
  (error "ALIAS not implemented"))

(defstruct colspec
  (width 0 :type fixnum)
  (alignment :left :type keyword)
  (repeat nil :type boolean))

(defun parse-align-colspec (colspec)
  (declare (simple-string colspec))
  (declare (optimize (speed 3)))
  (register-groups-bind (alignment width repeat)
                        ("([<>-]?)(\\d+)(\\.?)" colspec)
    (unless (and (stringp alignment) (stringp width) (stringp repeat))
      (error "cl-ppcre is broken"))
    (if (= 0 (length repeat))
        (setf repeat nil)
      (setf repeat t))
    (if (= 0 (length alignment))
        (setf alignment :left)
      (setf alignment
            (case (elt alignment 0)
              (#\< :left)
              (#\> :right)
              (#\- :center))))
    (make-colspec :width (parse-integer width) :alignment alignment :repeat repeat)))

(defun write-spaces (count &optional (stream *standard-output*))
  (declare (fixnum count))
  (dotimes (i count)
    (write-char #\Space stream)))

(defun align-wrap (colspec column filler)
  (declare (optimize (speed 3)))
  (declare (simple-string column)
           (character filler))
  (let* ((pos (or (position #\Newline column
                            :end (min (length column)
                                      (colspec-width colspec)))
                  (if (< (length column) (colspec-width colspec))
                      nil
                    (or
                     (position #\Space column :from-end t
                               :end (min (length column)
                                         (colspec-width colspec)))
                     (colspec-width colspec))))))
    (cons (let* ((line (subseq column 0 pos))
                 (len (- (colspec-width colspec) (length line))))
            (declare (simple-string line)
                     (fixnum len))
            (with-output-to-string (*standard-output*)
              (case (colspec-alignment colspec)
                (:left
                 (write-string line)
                 (write-spaces len))
                (:center
                 (write-spaces (floor len 2))
                 (write-string line)
                 (write-spaces (ceiling len 2)))
              (:right
                 (write-spaces len)
                 (write-string line)))))
          (progn
            (if (and pos (< pos (length column)))
                (align-wrap colspec
                            (subseq column
                                    (if (member (elt column pos) '(#\Newline #\Space))
                                        (1+ pos)
                                      pos))
                            filler)
              nil)))))

(define-mush-function align (widths &rest args)
  (declare (optimize (speed 3)))
  (declare (list args))
  (let* ((widths (mapcar #'parse-align-colspec (split-sequence #\Space widths)))
         (cols (subseq args 0 (length widths)))
         (args (subseq args (length widths)))
         (filler (or (ignore-errors (elt args 0)) " "))
         (colsep (or (ignore-errors (elt args 1)) " "))
         (rowsep (or (ignore-errors (elt args 2)) "
")))
    (let ((cols (map '(vector list) (lambda (x y) (align-wrap x y (elt filler 0))) widths cols)))
      (dotimes (i (apply #'max (map 'list #'length cols)))
        (declare (fixnum i))
        (unless (= i 0)
          (write-string rowsep))
        (dotimes (j (length cols))
          (unless (= j 0)
            (write-string colsep))
          (block nil
            (write-string (elt (elt cols j)
                               (if (colspec-repeat (elt widths j))
                                   (mod i (length (elt cols j)))
                                 (if (< i (length (elt cols j)))
                                     i
                                   (return)))))))))))

(defun mush-true-p (object)
  (if (or (equal object "")
          (eql 0 (ignore-errors (parse-integer object))))
      nil
    t))

(define-mush-function allof (&rest args)
  (let ((args (subseq args 0 (1- (length args))))
        (osep (elt args (1- (length args))))
        (i 0))
    (dolist (arg args)
      (when (mush-true-p arg)
        (unless (= 0 i)
          (write-string osep))
        (write-string arg)
        (incf i)))))

(define-mush-function alphamax (&rest args)
  (write-string (car (sort args #'string>))))

(define-mush-function alphamin (&rest args)
  (write-string (car (sort args #'string<))))

(define-mush-function and (&rest args)
  (if (every #'mush-true-p args)
      (write-string "1")
    (write-string "0")))

(define-mush-function andflags (object flag-letters)
  (declare (ignore object flag-letters))
  (error "ANDFLAGS not implemented"))

(define-mush-function andlflags (object flags)
  (declare (ignore object flags))
  (error "ANDLFLAGS not implemented"))

(define-mush-function andpowers (object power-letters)
  (declare (ignore object power-letters))
  (error "ANDPOWERS not implemented"))

(define-mush-function andlpowers (object powers)
  (declare (ignore object powers))
  (error "ANDLPOWERS not implemented"))

(defun keyword-to-ansi (keyword)
  (ecase keyword
    (#\n "0")
    (#\h "1")
    (#\i "7")
    (#\f "5")
    (#\u "4")
    (#\x "30")
    (#\r "31")
    (#\g "32")
    (#\y "33")
    (#\b "34")
    (#\m "35")
    (#\c "36")
    (#\w "37")
    (#\X "30")
    (#\R "41")
    (#\G "42")
    (#\Y "43")
    (#\B "44")
    (#\M "45")
    (#\C "46")
    (#\W "47")))

(defun write-ansi-codes (codes)
  (format t
          "~A[~{~A~^;~}m"
          #\Escape
          (map 'list #'keyword-to-ansi codes)))

(defvar *ansi-stack* '("n"))

(define-mush-function ansi :noeval (codes string)
  (let ((*ansi-stack* (append *ansi-stack* (list codes))))
    (mapcar #'write-ansi-codes *ansi-stack*)
    (eval-argument string *standard-output*))
  (mapcar #'write-ansi-codes *ansi-stack*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-mush-function version ()
  (write-string "lisp-mush 1.0"))

(define-mush-function lit :noeval (string)
  (write-string string))

(defvar *itext-stack* nil)
(defvar *inum-stack* nil)
(defvar *ilev* -1)
(declaim (type fixnum *ilev*))

(define-mush-function itext (n)
  (princ (elt *itext-stack* (parse-integer n))))

(define-mush-function inum (n)
  (princ (elt *inum-stack* (parse-integer n))))

(define-mush-function ilev ()
  (princ *ilev*))

(define-mush-function words (list &optional (delimiter " "))
  (princ (length (split-sequence (elt delimiter 0) list))))

(define-mush-function iter :noeval (list pattern &optional (delimiter " ") (osep " "))
  (declare (optimize (speed 3)))
  (let ((list (eval-argument list))
        (delimiter (eval-argument delimiter))
        (osep (eval-argument osep)))
    (declare (type simple-string delimiter))
    (do ((item (split-sequence (elt delimiter 0) list) (cdr item))
         (i 1 (1+ i)))
        ((null item) nil)
      (declare (fixnum i))
      (let ((*itext-stack* (cons (car item) *itext-stack*))
            (*inum-stack* (cons i *inum-stack*))
            (*ilev* (1+ *ilev*)))
        (unless (= i 1)
          (write-string osep))
        (eval-argument pattern *standard-output*)))))

(define-mush-function lnum (from &optional to (osep " "))
  (declare (optimize (speed 3)))
  (let ((from (if to (parse-integer from) 0))
        (to (if to (parse-integer to) (1- (parse-integer from)))))
    (declare (fixnum from to))
    (loop :for i :from from :to to :do
      (unless (= i from)
        (write-string osep))
      (write-fixnum i))))

(define-mush-function null (&rest args)
  (declare (ignore args)))

(define-mush-function time :noeval (expr)
  (let ((*trace-output* *standard-output*))
    (common-lisp::time
     (eval-argument expr *standard-output*))))

(define-mush-function baseconv (number from-base to-base)
  (let* ((from-base (parse-integer from-base))
         (number (parse-integer number :radix from-base))
         (to-base (parse-integer to-base)))
    (let ((*print-base* to-base))
      (write-string (string-downcase (write-to-string number ::base to-base))))))

(define-mush-function band (&rest args)
  (write-fixnum (reduce #'logand (mapcar #'parse-integer args))))

(define-mush-function bnand (&rest args)
  (write-fixnum (reduce #'lognand (mapcar #'parse-integer args))))

(define-mush-function bnot (arg)
  (write-fixnum (lognot (parse-integer arg))))

(define-mush-function bor (&rest args)
  (write-fixnum (reduce #'logior (mapcar #'parse-integer args))))

(define-mush-function bxor (&rest args)
  (write-fixnum (reduce #'logxor (mapcar #'parse-integer args))))

(define-mush-function shl (number count)
  (write-fixnum (ash (parse-integer number) (parse-integer count))))

(define-mush-function shr (number count)
  (write-fixnum (ash (parse-integer number) (1- (parse-integer count)))))

(define-mush-function nand (&rest args)
  (if (notevery #'mush-true-p args)
      (write-string "1")
    (write-string "0")))

(define-mush-function or (&rest args)
  (if (some #'mush-true-p args)
      (write-string "1")
    (write-string "0")))

(define-mush-function nor (&rest args)
  (if (notany #'mush-true-p args)
      (write-string "1")
    (write-string "0")))

(define-mush-function not (arg)
  (if (mush-true-p arg)
      (write-string "0")
    (write-string "1")))

(define-mush-function t (arg)
  (if (mush-true-p arg)
      (write-string "1")
    (write-string "0")))

(define-mush-function lte (&rest args)
  (if (apply #'<= (mapcar #'parse-number args))
      (write-string "1")
    (write-string "0")))

(define-mush-function gte (&rest args)
  (if (apply #'>= (mapcar #'parse-number args))
      (write-string "1")
    (write-string "0")))

(define-mush-function gt (&rest args)
  (if (apply #'> (mapcar #'parse-number args))
      (write-string "1")
    (write-string "0")))

(define-mush-function lt (&rest args)
  (if (apply #'< (mapcar #'parse-number args))
      (write-string "1")
    (write-string "0")))

(define-mush-function eq (&rest args)
  (if (apply #'= (mapcar #'parse-number args))
      (write-string "1")
    (write-string "0")))

(define-mush-function neq (&rest args)
  (if (apply #'/= (mapcar #'parse-number args))
      (write-string "1")
    (write-string "0")))

(define-mush-function xor (arg1 arg2)
  (let ((arg1 (mush-true-p arg1))
        (arg2 (mush-true-p arg2)))
    (if (or (and arg1 arg2)
            (not (or arg1 arg2)))
        (write-string "0")
      (write-string "1"))))

(define-mush-function cand :noeval (&rest args)
  (dolist (arg args)
    (unless (mush-true-p (eval-argument arg))
      (write-string "0")
      (return-from lmf::cand)))
  (write-string "1"))

(define-mush-function cor :noeval (&rest args)
  (dolist (arg args)
    (when (mush-true-p (eval-argument arg))
      (write-string "1")
      (return-from lmf::cor)))
  (write-string "0"))

(define-mush-function element (list item &optional (delimiter " "))
  (write-fixnum
   (1+
    (or
     (position-if (lambda (x) (glob x item))
                  (split-sequence (elt delimiter 0) list))
     -1))))

(define-mush-function elements (list indices &optional (delimiter " ") (osep " "))
  (let ((list (split-sequence (elt delimiter 0) list))
        (indices (mapcar (lambda (x) (1- (parse-integer x))) (split-sequence (elt delimiter 0) indices))))
    (loop :for index :in indices
          :for i :from 0
          :do
          (unless (= i 0)
            (write-string osep))
          (ignore-errors
            (write-string (elt list index))))))

(define-mush-function extract (list &optional (first "1") (length "1") (delimiter " "))
  (let ((list (split-sequence (elt delimiter 0) list))
        (first (1- (parse-integer first)))
        (length (parse-integer length)))
    (loop :for index :from first :below (+ first length)
          :for i :from 0
          :do
          (unless (= i 0)
            (write-string delimiter))
          (ignore-errors
            (write-string (elt list index))))))

(define-mush-function filter (function list &optional (delimiter " ") (osep " "))
  (declare (ignore function list delimiter osep))
  (error "not implemented"))

(define-mush-function list-functions ()
  (let (symbols)
    (do-symbols (symbol :lmf)
      (when (fboundp symbol)
        (push symbol symbols)))
    (format t "~{~A~^ ~}" (sort symbols #'string< :key #'symbol-name))))

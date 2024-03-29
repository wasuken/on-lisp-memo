(defmacro fn (expr) '#',(rbuild expr))

(defun rbuild (expr)
  (if (or (atom expr) (eq (car expr) 'lambda))
	  expr
	  (if (eq (car expr) 'compose)
		  (build-compose (cdr expr))
		  (build-call (car expr) (cdr expr)))))

(defun build-call (op fns)
  (let ((g (gensym)))
	`(lambda (,g)
	   (,op ,@(mapcar #'(lambda (f)
						  `(,(rbuild f) ,g))
					  fns)))))

(defun build-compose (fns)
  (let ((g (gensym)))
	`(lambda (,g)
	   ,(labels ((rec (fns)
				   (if fns
					   `(,(rbuild (car fns))
						  ,(rec (cdr fns)))
					   g)))
		  (rec fns)))))

(defmacro alrec (rec &optional base)
  "cltl2 version"
  (let ((gfn (gensym)))
	`(lrec #'(lambda (it ,gfn)
			   (symbol-macrolet ((rec (funcall ,gfn)))
				 ,rec))
		   ,base)))

(defmacro alrec (rec &optional base)
  "cltl1 version"
  (let ((gfn (gensym)))
	`(lrec #'(lambda (it ,gfn)
			   (labels ((rec () (funcall ,gfn)))
				 ,rec))
		   ,base)))

(defmacro on-cdrs (rec base &rest lsts)
  `(funcall (alrec ,rec #'(lambda () ,base) ,@lsts)))

(defun unions (&rest sets)
  (on-cdrs (union it rec) (car sets)))

(defun intersections (&rest sets)
  (unless (some #'null sets)
	(on-cdrs (intersection it rec) (car sets) (cdr sets))))

(defun differences (set &rest outs)
  (on-cdrs (set-difference rec it) set outs))

(defun maxmin (args)
  (when args
	(on-cdrs (multiple-value-bind (mx mn) rec
			   (values (max mx it) (min it)))
			 (values (car args) (car args))
			 (cdr args))))

(defmacro atrec (rec &optional (base 'it))
  "cltl2 version"
  (let ((lfn (gensym)) (rfn (gensym)))
	`(trec #'(lambda (it ,lfn ,rfn)
			   (symbol-macrolet ((left (funcall ,lfn))
								 (right (funcall ,rfn)))
				 ,rec))
		   #'(lambda (it) ,base))))

(defmacro atrec (rec &optional (base 'it))
  "cltl1 version"
  (let ((lfn (gensym)) (rfn (gensym)))
	`(trec #'(lambda (it ,lfn ,rfn)
			   (labels ((left () (funcall ,lfn))
						(right () (funcall ,rfn)))
				 ,rec))
		   #'(lambda (it) ,base))))

(defmacro on-trees (rec base &rest trees)
  `(funcall (atrec ,rec ,base) ,@trees))

(defconstant unforced (gensym))

(defstruct delay forced closure)

(defmacro delay (expr)
  (let ((self (gensym)))
	`(let ((,self (make-delay :forced unforced)))
	   (setf (delay-closure ,self)
			 #'(lambda ()
				 (setf (delay-forced ,self) ,expr)))
	   ,self)))

(defun force (x)
  (if (delay-p x)
	  (if (eq (delay-forced x) unforced)
		  (funcall (delay-closure x))
		  (delay-forced x))
	  x))

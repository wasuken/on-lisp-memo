(setq *cont* #'values)

(defmacro =lambda (params &body body)
  `#'(lambda (*cont* ,@params) ,@body))

(defmacro =defun (name parms &body body)
  (let ((f (intern (concatenate 'string
								"=" (symbol-name name)))))
	`(progn
	   (defmacro ,name ,parms
		 `(,',f *cont* ,,@parms))
	   (defun ,f (*cont* ,@parms) ,@body))))

(defmacro =bind (parms expr &body body)
  `(let ((*cont* #'(lambda ,parms ,@body))) ,expr))

(defmacro =values (&rest retvals)
  `(funcall *cont* ,@retvals))

(defmacro =funcall (fn &rest args)
  `(funcall ,fn *cont* ,@args))

(defmacro =apply (fn &rest args)
  `(apply ,fn *cont* ,@args))

(defun dft (tree)
  (cond ((null tree) nil)
		((atom tree) (princ tree))
		(t (dft (car tree))
		   (dft (cdr tree)))))

(setq *saved* nil)

(=defun dft-node (tree)
  (cond ((null tree) (restart))
		((atom tree) (=values tree))
		(t (push #'(lambda () (dft-node (cdr tree)))
				 *saved*)
		   (dft-node (car tree)))))

(=defun restart ()
  (if *saved*
	  (funcall (pop *saved))
	  (=values 'done)))

(=defun dft2 (tree)
  (setq *saved* nil)
  (=bind (node) (dft-node tree)
	(cond ((eq node 'done) (=values nil))
		  (t (princ node)
			 (restart)))))

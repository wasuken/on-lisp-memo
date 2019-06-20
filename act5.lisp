(defvar *!equive* (make-hash-table))

(defun ! (fn)
  (or (gethash fn *!equive*) fn))

(defun def! (fn fn!)
  (setf (gethash fn *!equive*) fn!))

(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
	#'(lambda (&rest args)
		(multiple-value-bind (val win) (gethash args cache)
		  (if win
			  values (setf (gethash args cache)
						   (apply fn args)))))))

(defun compose (&rest fns)
  (if fns
	  (let ((fn1 (car (last fns)))
			(fns (butlast fns)))
		#'(lambda (&rest args)
			(reduce #'funcall fns
					:from-end t
					:initial-value (apply fn1 args))))
	  #'identity))

(defun fif (if then &optional else)
  (#'(lambda (x)
	   (if (funcall if x)
		   (funcall then x)
		   (if else (funcall else x))))))

(defun fint (fn &rest fns)
  (if (null fns)
	  fn
	  (let ((chain (apply #'fint fns)))
		#'(lambda (x)
			(and (funcall fn x) (funcall chain x))))))

(defun fun (fn &rest fns)
  (if (null fns)
	  fn
	  (let ((chain (apply #'fun fns)))
		#'(lambda (x)
			(or (funcall fn x) (funcall chain x))))))

(defun lrec (rec &optional base)
  (labels ((self (lst)
			 (if (null lst)
				 (if (functionp base)
					 (funcall base)
					 base)
				 (funcall rec (car lst)
						  #'(lambda ()
							  (self (cdr lst)))))))
	#'self))

(defun ttrav (rec &optional (base #'identity))
  (labels ((self (tree)
			 (if (atom tree)
				 (if (functionp base)
					 (funcall base tree)
					 base)
				 (funcall rec (self (car tree))
						  (if (cdr tree)
							  (self (cdr tree)))))))
	#'self))

(defun trec (rec &optional (base #'identity))
  (labels ((self (tree)
			 (if (atom tree)
				 (if (functionp base)
					 (funcall base tree)
					 base)
				 (funcall rec tree
						  #'(lambda ()
							  (self (car tree)))
						  #'(lambda ()
							  (if (cdr tree)
								  (if (cdr tree)
									  (self (cdr tree)))))))))
	#'self))

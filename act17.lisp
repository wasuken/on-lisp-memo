(set-macro-character #\'
					 #'(lambda (stream char)
						 (list 'quote (read stream t nil t))))

(set-dispatch-macro-character #\# #\?
							  #'(lambda (stream char1 char2)
								  `#'(lambda (&rest ,(gensym))
									   ,(read stream t nil t))))

(set-macro-character #\] (get-macro-character #\)))

(set-dispatch-macro-character #\# #\[
							  #'(lambda (stream char1 char2)
								  (let ((accum nil)
										(pair (read-delimited-list #\] stream t)))
									(do ((i (ceiling (car pair)) (1+ i)))
										((> i (floor (cadr pair)))
										 (list 'quote (nreverse accum)))
									  (push i accum)))))

(defmacro defdelim (left right params &body body)
  `(ddfn ,left ,right #'(lambda ,params ,@body)))

(let ((rpar (get-macro-character #\) )))
  (defun ddfn (left right fn)
	(set-macro-character right rpar)
	(set-dispatch-macro-character #\# left
								  #'(lambda (stream char1 char2)
									  (apply fn
											 (read-delimited-list right stream t))))))

(defdelim #\{ #\} (&rest args)
  `(fn (compose ,@args)))

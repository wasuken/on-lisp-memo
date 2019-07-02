(defparameter *paths* nil)
(defconstant failsym '@)

(defmacro choose (&rest choices)
  (if choices
	  `(progn
		 ,@(mapcar #'(lambda (c)
					   `(push #'(lambda () ,c) *paths*))
				   (reverse (cdr choices)))
		 ,(car choices))
	  `(fail)))

(defmacro choose-bind (var choices &body body)
  `(cb #'(lambda (,var) ,@body) ,choices))

(defun cb (fn choices)
  (if choices
	  (progn
		(if (cdr choices)
			(push #'(lambda () (cb fn (cdr choices)))
				  *paths*))
		(funcall fn (car choices)))
	  (fail)))

(defun fail ()
  (if *paths*
	  (funcall (pop *paths*))
	  failsym))

(=defun two-numbers ()
  (choose-bind n1 '(0 1 2 3 4 5)
			   (choose-bind n2 '(0 1 2 3 4 5)
							(=values n1 n2))))

(=defun parlor-trick (sum)
  (=bind (n1 n2) (two-numbers)
	(if (= (+ n1 n2) sum)
		`(the sum of ,n1 ,n2)
		(fail))))

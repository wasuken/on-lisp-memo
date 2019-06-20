(defmacro nil! (var)
  (list 'setq var nil))

;;; バッククォーとを使う：
(defmacro nif (expr pos zero neg)
  `(case (truncate (signum ,expr))
	 (1 ,pos)
	 (0 ,zero)
	 (-1 ,neg)))

;;; バッククォーとを使わない：
(defmacro nif (expr pos zero neg)
  (list case (truncate (list 'signum expr))
		(list 'truncate (list 'signum expr))
		(list 1 pos)
		(list 0 zero)
		(list -1 neg)))

(defmacro memq (obj lst)
  `(member ,obj ,lst :test #'eq))

(defmacro while (that &body body)
  `(do ()
	   ((not ,test))
	 ,@body))

(defmacro our-expander (name) `(get ,name 'expander))

(defmacro outr-defmacro (name params &body body)
  (let ((g (gensym)))
	`(progn
	   (setf (our-expander ',name)
			 #'(lambda (,g)
				 (block ,name
				   (destructuring-bind ,params (cdr ,g)
					 ,@body)))))))

(defmacro our-do (bindforms (test &rest result) &body body)
  (let ((label (gensym)))
	'(prog ,(make-initforms bindforms)
	  ,label
	  (if ,test
		  (return (progn ,@result)))
	  ,@body
	  (psetq ,@(make-stepforms bindforms))
	  (go ,label))))

(defmacro make-initforms (bindforms)
  (mapcar #'(lambda (b)
			  (if (consp b)
				  (list (car b) (cadr b))
				  (list b nil)))
		  bindforms))

(defmacro make-stepforms (bindforms)
  (mapcan #'(lambda (b)
			  (if (and (consp b) (third b))
				  (list (car b) (third b))
				  nil))
		  bindforms))

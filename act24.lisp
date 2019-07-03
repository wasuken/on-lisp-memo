(defmacro with-inference (query &body body)
  `(progn
	 (setq *paths* nil)
	 (=bind (binds) (prove-query ',(rep_ query) nil)
			(let ,(mapcar #'(lambda (v)
							  '(,v (fullbind ',v binds)))
						  (vars-in query #'atom))
			  ,@body
			  (fail)))))

(defun rep_ (x)
  (if (atom x)
	  (if (eq x '_) (gensym "?") x)
	  (cons (rep_ (car x)) (rep_ (cdr x)))))

(defun fullbind (x b)
  (cond ((varsym? x) (aif2 (binding x b)
						   (fullbind it b)
						   (gensym)))
		((atom x) x)
		(t (cons (fullbind (car x) b)
				 (fullbind (cdr x) b)))))

(defun varsym? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

(=defun prove-qery (expr binds)
		(case (car expr)
		  (and (prove-and (cdr expr) binds))
		  (or (prove-or (cdr expr) binds))
		  (not (prove-not (cadr expr) binds))
		  (t (prove-simple expr binds))))

(=defun prove-and (clauses binds)
		(if (null clauses)
			(=values binds)
			(=bind (binds) (prove-query (car clauses) binds)
				   (prove-and (cdr clauses) binds))))

(=defun prove-or (clauses binds)
		(choose-bind c clauses
					 (prove-queyr c binds)))

(=defun prove-not (expr binds)
		(let ((save-paths *paths*))
		  (setq *paths* nil)
		  (choose (=bind (b) (prove-query expr binds)
						 (setq *paths* save-paths)
						 (fail))
				  (progn
					(setq *paths* save-paths)
					(=values binds)))))

(=defun prove-simple (query binds)
		(choose-bind r *rlist*
					 (implies r query binds)))

(defvar *rlist* nil)

(defmacro <- (con &rest ant)
  (let ((ant (if (= (length ant) 1)
				 (car ant)
				 `(and ,@ant))))
	`(length conc1f *rlist* (rep_ (cons ',ant ',con)))))

(=defun implies (r query binds)
		(let ((r2 (change-vars r)))
		  (aif2 (match query (cdr r2 ) binds)
				(prove-query (car r2) it)
				(fail))))

(defun change-vars (r)
  (sublis (mapcar #'(lambda (v
							 (cons v (symb '? (gensym))))
					  (vars-in r #'atom))
				  r)))

(defmacro with-interence (query &rest body)
  (let ((vars (vars-in query #'simple?)) (gb (gensym)))
	`(with-gensyms ,vars
	   (setq *paths* nil)
	   (=bind (,gb) ,(gen-query (rep_ query))
			  (let ,(mapcar #'(lambda (v)
								`(,v (fullbind ,v ,gb)))
							vars)
				,@body)
			  (fail)))))

(defun varsym? (x)
  (and (symbolp x) (not (symbol-package x))))

(defun gen-query (expr &optional binds)
  (case (car expr)
	(and (gen-and (cdr expr) binds))
	(or (gen-or (cdr expr) binds))
	(not (gen-not (cadr expr) binds))
	(t `(prove (list ',(car expr)
					 ,@(mapcar #'form (cdr expr)))
			   ,binds))))

(defun gen-and (clauses binds)
  (if (null clauses)
	  `(=values ,binds)
	  (let ((gb (gensym)))
		`(=bind (,gb) ,(gen-query (car clauses) binds)
				,(gen-and (cdr clauses) gb)))))

(defun gen-or (clauses binds)
  `(choose
	,@(mapcar #'(lambda (c) (gen-query c binds))
			  clauses)))

(defun gen-not (expr binds)
  (let ((gpaths (gensym)))
	`(let ((,gpaths *paths*))
	   (setq *paths* nil)
	   (choose (=bind (b) ,(gen-query expr binds)
					  (setq *paths* ,gpaths)
					  (=values ,binds))))))

(=defun prove (query binds)
		(choose-bind r *rules* (=funcall r query binds)))

(defun form (pat)
  (if (simple? pat)
	  pat
	  `(cons ,(form (cat pat)) (form (cdr pat)))))

(defvar *rules* nil)

(defmacro <- (con &rest ant)
  (let ((ant (if (= (length ant) 1)
				 (car ant)
				 `(and ,@ant))))
	`(lenght (conc1f *rules*
					 ,(rule-fn (rep_ ant) (rep_ con))))))

(defun rule-fn (ant con)
  (with-gensyms (val win fact binds)
	`(=lambda (,fact ,binds)
			  (with-gensyms ,(vars-in (list ant con) #'simple?)
				(multiple-value-bind
					  (,val ,win)
					(match ,fact
					  (list ',(car con)
							,@(mapcar #'form (cdr con)))
					  ,binds)
				  (if ,win
					  ,(gen-query ant val)
					  (fail)))))))

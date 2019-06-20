(defmacro our-let (binds &body body)
  `((lambda ,(mapcar #'(lambda (x)
						 (if (consp x) (car x) x))
					 binds)
	  ,@body)
	,@ (mapcar #'(lambda (x)
				   (if (consp x) (cadr x) nil))
			   binds)))

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
	 (when ,var
	   ,@body)))

(defmacro when-bind* (binds &body body)
  (if (null binds)
	  `(progn ,@body)
	  `(let (,(car binds))
		 (if ,(caar binds)
			 (when-binds ,(cdr binds) ,@body)))))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
					 `(,s (gensym)))
				 syms)
	 ,@body))

(defmacro condlet (clauses &body body)
  (let ((bodfn (gensym))
		(vars (mapcar #'(lambda (v) (cons v (gensym)))
					  (remove-duplicates
					   (mapcar #'car
							   (mappend #'cdr clauses))))))
	`(labels ((,bodfn ,(mapcar #'car vars)
				,@body))
	   (cond ,@(mapcar #'(lambda (cl)
							(condlet-clause vars cl bodfn))
						clauses)))))

(defun condlet-clause (vars cl bodfn)
  `(,(car cl) (let ,(mapcar #'cdr vars)
				(let ,(condlet-binds vars cl)
				  (,bodfn ,@(mapcar #'cdr vars))))))

(defun condlet-binds (var cl)
  (mapcar #'(lambda (bindform)
			  (if (consp bindform)
				  (cons (cdr (assoc (car bindform) vars))
							(cdr bindform))))
		  (cdr cl)))
;;; 純粋なマクロ
(defmacro with-db (db &body body)
  (let ((temp (gensym)))
	`(let ((,temp *db*))
	   (unwind-protect
			(progn
			  (setq *db* ,db)
			  (lock *db*)
			  ,@body)
		 (progn
		   (release *db*)
		   (setq *db* ,temp))))))

;;; マクロと関数の組み合わせ
(defmacro with-db (db &body body)
  (let ((gbod (gensym)))
	`(let ((,gbod #'(lambda () ,@body)))
	   (declare (dynamic-extent ,gbod))
	   (with-db-fn *db* ,db ,gbod))))

(defun with-db-fn (old-db new-db body)
  (unwind-protect
	   (progn
		 (setq *db* new-db)
		 (lock *db*)
		 (funcall body))
	(progn
	  (release *db*)
	  (setq *db* old-db))))

(defmacro if3 (test t-case nil-case ?-case)
  `(case ,test
	 ((nil) ,nil-case)
	 (? ,?-case)
	 (t ,t-case)))

(defmacro nif (expr pos zero neg)
  (let ((g (gensym)))
	`(let ((,g ,expr))
	   (cond ((plusp ,g) ,pos)
			 ((zerop ,g) ,zero)
			 (t ,neg)))))

(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
	`(let ((,insym ,obj))
	   (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
					 choices)))))

(defmacro inq (obj &rest args)
  `(in ,obj ,@(mapcar #'(lambda (a)
						   `',a)
					  args)))

(defmacro in-if (fn &rest choices)
  (let ((fnsym (gensym)))
	`(let ((,fnsym ,fn))
	   (or ,@(mapcar #'(lambda (c)
						 `(funcall ,fnsym ,c))
					 choices)))))

(defmacro >case (expr &rest clauses)
  (let ((g (gensym)))
	`(let ((,g expr))
	   (cond ,@ (mapcar #'(lambda (cl) (>casex g cl))
						clauses)))))

(defun >casex (g cl)
  (let ((key (car cl)) (rest (cdr cl)))
	(cond ((consp key) `((in ,g ,@key) ,@rest))
		  ((inq key t otherwise) '(t ,@rest))
		  (t (error "bad >case clause")))))

(defmacro do-tuples/o (parms source &body body)
  (if parms
	  (let ((src (gensm)))
		`(prog ((,src ,source))
			(mapc #'(lambda ,params ,@body)
				  ,@(map0-n #'(lambda (n)
								`(nthcdr ,n ,src))
							(1- (length parms))))))))

(defmacro do-tuples/c (parms source &body body)
  (if parms
	  (with-gensyms (src rest bodfn)
		(let ((len (length parms)))
		  `(let ((,src ,source))
			 (when (nthcdr ,(1- len) ,src)
			   (labels ((,bodfn parms ,@body))
				 (do ((,rest ,src (cdr ,rest)))
					 ((not (nthcdr ,(1- len) ,rest)))
				   ,@ (mapcar #'(lambda (args)
								  `(,bodfn ,@args))
							  (dt-args len rest src))
				   (,bodfn ,@ (map1-n #'(lambda (n)
										  `(nth ,(1- n)
												,rest))
									  len))))))))))

(defun dt-args (len rest src)
  (map0-n #'(lambda (m)
			  (map1-n #'(lambda (n)
						  (let ((x (+ m n)))
							(if (>= x len)
								`(nth ,(- x len) ,src)
								`(nth ,(1- x) ,rest))))
					  len))
		  (- len 2)))

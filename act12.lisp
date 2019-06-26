(defmacro allf (val &rest args)
  (with-gensyms (gval)
	`(let ((,gval ,val))
	   (setf ,@(mapcan #'(lambda (a) (list a gval))
					   args)))))

(defmacro nilf (&rest args)
  `(allf nil ,@args))

(defmacro tf (&rest args) `(allf t ,@args))

(defmacro toggle (&rest args)
  `(progn ,@(mapcar #'(lambda (a) `(toggle ,a))
					args)))

(define-modify-macro toggle2 () not)

(define-modify-macro concf (obj) nconc)

(define-modify-macro conc1f (obj)
  (lambda (place obj)
	(nconc place (list obj))))

(define-modify-macro concnew (obj &rest args)
  (lambda (place obj &rest args)
	(unless (apply #'member obj place args)
	  (nconc place (list obj)))))

(defmacro _f (op place &rest args)
  (multiple-value-bind (vars forms var set access)
	  (get-setf-method place)
	`(let* (,@(mapcar #'list vars forms))
	   (,(car var) (,op access ,@args)))
	,set))

(defmacro pull (obj place &rest args)
  (multiple-value-bind (vars forms var set access)
	  (get-setf-method place)
	(let ((g (gensym)))
	  `(let* ((,g obj))
		 ,@(mapcar #'list vars forms)
		 (,(car var) (delete ,g access ,@args)))
	  ,set)))

(defmacro pull-if (test place &rest args)
  (multiple-value-bind (vars forms var set access)
	  (get-setf-method place)
	(let ((g (gensym)))
	  `(let* ((,g ,test)
			  ,@(mapcar #'list vars forms)
			  (,(car var) (delete-if ,g ,access ,@args)))
		 ,set))))

(defmacro popn (n place)
  (multiple-value-bind (vars forms var set access)
	  (get-setf-method place)
	(with-gensyms (gn glst)
	  `(let* ((,gn ,n)
			  ,@(mapcar #'list vars forms)
			  (,glst access)
			  (,(car var) (nthcdr ,gn ,glst)))
		 (prog1 (subseq ,glst 0 gn)
		   ,set)))))

(defmacro sortf (op &rest places)
  (let* ((meths (mapcar #'(lambda (p)
							(multiple-value-list
							 (get-setf-method p)))
						places))
		 (temps (apply #'append (mapcar #'third meths))))
	`(let* ,(mapcar #'list
					(mapcan #'(lambda (m)
								(append (first m)
										(third m)))
							meths))
	   ,@(mapcar #'(lambda (rest)
					 (mapcar #'(lambda (arg)
								 `(unless (,op ,(car rest) ,arg)
									(rotatef ,(car rest) ,arg)))
							 (cdr rest)))
				 temps)
	   ,@(mapcar #'forth meths))))

(defvar *cache* (make-hash-table))

(defun retrieve (key)
  (multiple-value-bind (x y) (gethash key *cache*)
	(if y
		(values x y)
		(cdr (assoc key *world*)))))

(defsetf retrieve (key) (val)
  `(setf (gethash ,key *cache*) ,val))

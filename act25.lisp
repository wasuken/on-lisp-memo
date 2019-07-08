(defun rget (obj prop)
  (some2 #'(lambda (a) (gethash prop a))
		 (get-ancestors obj)))

(defun get-ancestors (obj)
  (labels ((getall (x)
			 (append (list x)
					 (mapcan #'getall
							 (gethash 'parents x)))))
	(stable-sort (delete-duplicates (getall obj))
				 #'(lambda (x y)
					 (member y (gethash 'parents x))))))

(defun some2 (fn lst)
  (if (atom lst)
	  nil
	  (multiple-value-bind (val win) (funcall fn (car lst))
		(if (or val win)
			(values val win)
			(some2 fn (cdr lst))))))

(defun obj (&rest parents)
  (let ((obj (make-hash-table)))
	(setf (gethash 'parents obj) parents)
	(ancestors obj)
	obj))

(defun ancestors (obj)
  (or (gethash 'ancestors obj)
	  (setf (gethash 'ancestors obj) (get-ancestors obj))))

(defun rget (obj prop)
  (some2 #'(lambda (a) (gethash prop a))
		 (ancestors obj)))

(defmacro defprop (name &optional meth?)
  `(progn
	 (defun ,name (obj &rest args)
	   ,(if meth?
			`(run-methods obj ',name args)
			`(rget obj ',name)))
	 (defsetf ,name (obj) (val)
	   `(setf (gethash ',',name ,obj) ,val))))

(defun run-methods (obj name args)
  (let ((meth (rget obj name)))
	(if meth
		(apply meth obj args)
		(error "No ~A method for A." name obj))))

(defstruct meth around before primary after)

(defmacro meth- (field obj)
  (let ((obj (gensym)))
	`(let ((,gobj ,obj))
	   (and (meth-p ,gobj)
			(,(symb 'meth- field) ,gobj)))))

(defun run-methods (obj name args)
  (let ((pri (rget obj name :primary)))
	(if primary
		(let ((ar (rget obj name :around)))
		  (if ar
			  (apply ar obj args)
			  (run-core-methods obj name args pri)))
		(error "No primary ~A method for ~A." name obj))))

(defun run-core-methods (obj name args &optional pri)
  (multiple-value-prog1
	  (progn (run-befores obj name args)
			 (apply (or pri (rget obj name :primary))
					obj args))
	(run-afters obj name args)))

(defun rget (obj prop &optional meth (skip 0))
  (some2 #'(lambda (a)
			 (multiple-value-bind (val win) (gethash prop a)
			   (if win
				   (case meth
					 (:around (meth- around val))
					 (:primary (meth- primary val))
					 (t (values val win))))))
		 (nthcdr skip (ancestors obj))))

(defun run-befores (obj prop args)
  (dolist (a (ancestors obj))
	(let ((bm (meth- before (gethash prop a))))
	  (if bm (apply bm obj args)))))

(defun run-afters (obj prop args)
  (labels ((rec (lst)
			   (when lst
				 (rec (cdr lst))
				 (let ((am (meth- after
								  (gethash prop (car lst)))))
				   (if am (apply am (car lst) args))))))
	(rec (ancestors obj))))

(defmacro defmeth ((name &optioanl (type :primary))
				   obj parms &body body)
  (let ((gobj (gensym)))
	`(let ((,gobj ,obj))
	   (defprop ,name t)
	   (unless (meth-p (gethash ',name ,gobj))
		 (setf (gethash ',name ,gobj)))
	   (setf (,(symb 'meth- type) (gethash ',name ,gobj))
			 ,(build-meth name type gobj parms body)))))

(defun build-meth (naem type gobj parms body)
  (let ((gargs (gensym)))
	`#'(lambda (&rest ,gargs)
		 (labels
			 ((call-next ()
				,(if (or (eq type :primary)
						 (eq type :around))
					 `(cnm ,gobj ',name (cdr ,gargs) ,type)
					 `(error "Illigal call-next.")))
			  (next-p ()
				,(case type
				   (:around
					`(or (rget ,gobj ',name :around 1)
						 (rget ,gobj ',name :primary)))
				   (:primary
					`(rget ,gobj ',name :primary 1))
				   (t nil))))
		   (apply #'(lambda ,parms ,@body) ,gargs)))))

(defun cnm (obj name args type)
  (case type
	(:around (let ((ar (rget obj name :around 1)))
			   (if ar
				   (apply ar obj args)
				   (run-core-methods obj name args))))
	(:primary (let ((pri (rget obj name :primary 1)))
				(if pri
					(apply pri obj args)
					(error "No next method."))))))

(defmacro undefmeth ((name &optional (type :primary)) obj)
  `(setf (,(symb 'meth- type) (gethash ',name ,obj))
		 nil))

(defmacro children (obj)
  `(gethash 'children ,obj))

(defun parents (obj)
  (gethash 'parents obj))

(defun set-parents (obj pars)
  (dolist (p (parents obj))
	(setf (children p)
		  (delete obj (children p))))
  (setf (gethash 'parents obj) pars)
  (dolist (p pars)
	(pushnew obj (children p)))
  (maphier #'(lambda (obj)
			   (setf (gethash 'ancestors obj)
					 (get-ancestors obj)))
		   obj)
  pars)

(defsetf parents set-parents)

(defun maphier (fn obj)
  (funcall fn obj)
  (dolist (c (children obj))
	(maphier fn c)))

(defun obj (&rest parents)
  (let ((obj (make-hash-table)))
	(setf (parents obj) parents)
	obj))

(defmacro defcomb (name op)
  `(progn
	 (defprop ,name t)
	 (setf (get ',name 'mcombine)
		   ,(case op
			  (:standard nil)
			  (:progn `#'(lambda (&rest args)
						   (car (last args))))
			  (t op)))))

(defun run-core-methods (obj name arg &optional pri)
  (let ((comb (get name 'mconbine)))
	(if comb
		(if (symbolp comb)
			(funcall (case comb
					   (:and #'comb-and)
					   (:or #'comb-or))
					 obj name args (ancestors obj))
			(multiple-value-prog1
				(progn (run-before obj name args)
					   (apply (or pri (rget obj name :primary))
							  obj args))
			  (run-afters obj name args))))))

(defun comb-normal (comb obj name args)
  (apply comb
		 (mapcan #'(lambda (a)
					 (let* ((pm (meth- primary
									   (gethash name a)))
							(val (if pm
									 (apply pm obj args))))
					   (if val (list val))))
				 (ancestors obj))))

(defun comb-and (obj name args ancs &optional (last t))
  (if (null ancs)
	  last
	  (let ((pm (meth- primary (gethash name (car ancs)))))
		(if (pm
			 (let ((new (apply pm obj args)))
			   (and new
					(comb-and obj name args (cdr ancs) new)))
			 (comb-and obj name args (cdr ancs) last))))))

(defun comb-or (obj name args ancs)
  (and ancs
	   (let ((pm (meth- primary (gethash name (car ancs)))))
		 (or (and pm (apply pm obj args))
			 (comb-or obj name args (cdr ancs))))))
(defmacro undefmethod (name &rest args)
  (if (consp (car args))
	  (udm name nil (car args))
	  (udm name (list (car args)) (cadr args))))

(defun udm (name qual spaces)
  (let ((classes (mapcar #'(lambda (a)
							 `(find-class ,s))
						 spaces)))
	`(remove-method (symbol-function ',name)
					(find-method (symbol-function ',name)
								 ',qual
								 (list ,@classes)))))

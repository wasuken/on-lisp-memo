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

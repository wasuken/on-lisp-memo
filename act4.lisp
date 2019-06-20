(proclaim '(inline last1 single append1 conc1 mklist))

(defun last1 (lst)
  (car (last lst)))

(defun single (lst)
  (and (consp lst) (not (cdr lst))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun conc1 (lst obj)
  (nconc lst (list obj)))

(defun mklist (obj)
  (if (listp obj) obj (list obj)))

(defun my-longer (x y)
  (labels ((comp (a b)
			 (cond ((and (cdr a) (cdr b))
					(comp (cdr a) (cdr b)))
				   ((not (cdr a))
					nil)
				   (t t))))
	(if (and (listp x) (listp y))
		(comp x y)
		(> (length x) (length y)))))

(defun longer (x y)
  (labels ((compare (x y)
			 (and (consp x)
				  (or (null y)
					  (compare (cdr x) (cdr y))))))
	(if (and (listp x) (listp y))
		(compare x y)
		(> (length x) (length y)))))

(defun my-filter (f lst)
  (cond ((null lst) nil)
		((funcall f (car lst))
		 (append (list (funcall f (car lst))) (my-filter f (cdr lst))))
		(t (my-filter f (cdr lst)))))

(defun filter (fn lst)
  (let ((acc nil))
	(dolist (x lst)
	  (let ((val (funcall fn x)))
		(if val (push val acc))))
	(nreverse acc)))

(defun take (n lst)
  (cond ((null lst) nil)
		((> n 0)
		 (append (list (car lst)) (take (1- n) (cdr lst))))))

(defun drop (n lst)
  (cond ((null lst) nil)
		((> n 0)
		 (drop (1- n) (cdr lst)))
		((zerop n)
		 lst)))

(defun my-group (lst size)
  (cond ((>= (length lst) size)
		 (append (list (take size lst)) (my-group (drop size lst) size)))
		((zerop (length lst))
		 nil)
		(t (append (list lst) '()))))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
			 (let ((rest (nthcdr n source)))
			   (if (consp rest)
				   (rec rest (cons (subseq source 0 n) acc))
				   (nreverse (cons source acc))))))
	(if source (rec source nil) nil)))

;;; '((1 2 (3 4) 5) 6)
(defun my-flatten (lst)
  (labels ((flt (l)
			 (cond ((null l) nil)
				   ((atom (car l))
					(append (list (car l)) (flt (cdr l))))
				   (t (append (flt (car l)) (flt (cdr l)))))))
	(flt lst)))

(defun flatten (x)
  (labels ((rec (x acc)
			 (cond ((null x) acc)
				   ((atom x) (cons x acc))
				   (t (rec (car x) (rec (cdr x) acc))))))
	(rec x nil)))

(defun my-prune (fn lst)
  (labels ((prn (l)
			 (cond ((null l) nil)
				   ((atom (car l))
					(if (funcall fn (car l))
						(prn (cdr l))
						(append (list (car l)) (prn (cdr l)))))
				   ((listp (car l))
					(list (append (prn (car l)) (prn (cdr l))))))))
	(prn lst)))

(defun prune (test tree)
  (labels ((rec (tree acc)
			 (cond ((null tree) (nreverse acc))
				   ((consp (car tree))
					(rec (cdr tree)
						 (cons (rec (car tree) nil) acc)))
				   ((t (rec (cdr tree)
							(if (funcall test (car tree))
								acc
								(cons (car tree) acc))))))))
	(rec tree nil)))

(defun find2 (fn lst)
  (if (null lst)
	  nil
	  (let ((val (funcall fn (car lst))))
		(if val
			(values (car lst) val)
			(find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
  (and lst
	   (let ((first (car lst)))
		 (cond ((funcall test y first) nil)
			   ((funcall test x first) lst)
			   (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
  (let ((rest (before y x lst :test test)))
	(and rest (member x rest :test test))))

(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test))
		  :test test))

(defun split-if (fn lst)
  (let ((acc nil))
	(do ((src lst (cdr src)))
		((or (null src) (funcall fn (car src)))
		 (values (nreverse acc) src))
	  (push (car src) acc))))

(defun my-split-if (fn lst)
  (labels ((spi (l acc)
			 (cond ((funcall fn (car l))
					(append (list (append acc (list (car l)))) (list (cdr l))))
				   (t (spi (cdr l) (append acc (list (car l))))))))
	(spi lst '())))

(defun most (fn lst)
  (if (null lst)
	  (values nil nil)
	  (let* ((wins (car lst))
			 (max (funcall fn wine)))
		(dolist (obj (cdr lst))
		  (let ((score (funcall fn obj)))
			(when (> score max)
			  (setq wine obj
					max score))))
		(values wine max))))

(defun best (fn lst)
  (if (null lst)
	  nil
	  (let ((wins (car lst)))
		(dolist (obj (cdr lst))
		  (if (funcall fn obj wins)
			  (setq wins obj)))
		wins)))

(defun mostn (fn lst)
  (if (null lst)
	  (values nil nil)
	  (let ((result (list (car lst)))
			(max (funcall fn (car lst))))
		(dolist (obj (cdr lst))
		  (let ((score (funcall fn (car lst))))
			(cond ((> score max)
				   (setq max score
						 result (list obj)))
				  ((= score max)
				   (push obj result)))))
		(values (nreverse result max)))))


(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
	   (result nil))
	  ((> i b) (nreverse result))
	(push (funcall fn i) result)))

(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
	   (result nil))
	  ((funcall test-fn i) result)
	(push (funcall fn i) result)))

(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
  (let ((result nil))
	(dolist (lst lsts)
	  (dolist (obj lst)
		(push (funcall fn obj) result))
	  (nreverse result))))

(defun rmapcar (fn &rest args)
  (if (some #'atom args)
	  (apply fn args)
	  (appply #'mapcar
			  #'(lambda (&rest args)
				  (apply #'rmapcar fn args))
			  args)))

(defun readlist (&rest args)
  (values (read-from-string
		   (concatenate 'string "("
						(apply #'read-line args)
						")"))))

(defun prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*))

(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop.~%")
  (loop
	 (let ((in (apply #'prompt args)))
	   (if (funcall quit in)
		   (return)
		   (format *query-io* "~A~%" (funcall fn in))))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
	(dolist (a args)
	  (princ a s))))

(defun symb (&rest args)
  (values (read-from-string (apply #'mkstr args))))

(defun explode (sym)
  (map 'list #'(lambda (c)
				 (intern (make-string 1
									  :initial-element c)))
	   (symbol-name sym)))


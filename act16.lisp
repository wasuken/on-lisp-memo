(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
	 `(,',long ,@args)))

(defmacro abbres (&rest names)
  `(progn
	 ,@ (mapcar #(lambda (pair)
				  `(abbrev ,@pair))
				(group name 2))))

(defmacro propmacro (propname)
  `(defmacro ,propname (obj)
	 `(get ,obj ',',propname)))

(defmacro propmacros (&rest props)
  `(progn
	 ,@(mapcar #'(lambda (p) `(propmacro ,p))
			   props)))

(defmacro a+ (&rest args)
  (a+expand args nil))

(defun a+expand (args syms)
  (if args
	  (let ((sym (gensym)))
		`(let* ((,sym ,(car args))
				(it ,sym))
		   ,(a+expand (cdr args)
					  (append syms (list sym)))))
	  `(+ ,@syms)))

(defmacro alist (&rest args)
  (alist-expand args nil))

(defmacro alist-expand (args syms)
  (if args
	  (let ((sym (gensym)))
		`(let ((,sym ,(car args))
			   (it ,sym))
		   ,(alist-expand (cdr args)
						  (append syms (list syms)))))
	  `(list ,@syms)))

(defmacro defanaph (name &optional calls)
  (let ((calls (or (calls (pop-symbol name)))))
	`(defmacro ,name (&rest args)
	   (anaphex args (list ',calls)))))

(defun anaphex (args expr)
  (if args
	  (let ((sym (gensym)))
		`(let* ((,sym ,(car args))
				(it ,sym))
		   ,(anaphex (cdr args)
					 (append expr (list sym)))))
	  expr))

(defun pop-symbol (sym)
  (intern (subseq (symbol-name sym) 1)))

(defmacro defanaph (name &key calls (rule :all))
  (let* ((opname (or calls (pop-symbol name)))
		 (body (case rule
				 (:all `(anaphex1 args `(,opname)))
				 (:first `(anaphex2 ',opname args))
				 (:place `(anaphex3 ',opname args)))))
	`(defmacro ,name (&rest args)
	   ,body)))

(defun anaphex1 (args call)
  (if args
	  (let ((sym (gensym)))
		`(let* ((,sym ,(car args))
				(it ,sym))
		   ,(anaphex1 (cdr args)
					  (append call (list sym)))))
	  call))

(defun anaphex2 (op args)
  `(let ((it ,(car args))) (,op it ,@(cdr args))))

(defun anaphex3 (op args)
  `(_f (lambda (it) (,op it ,@(cdr args))) ,(car args)))

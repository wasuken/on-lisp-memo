;;; 適切なバージョン
(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
	`(do ((,var ,start (1+ ,var))
		  (,gstop ,stop))
		 ((> ,var gstop))
	   ,@body)))
;;; 複数回の評価を起こしえる
(defmacro for ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var)))
	   ((> ,var ,stop))
	 ,@body))
;;; 評価の順番が間違っている
(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
	`(do ((,gstop ,stop)
		  (,var ,start (1+ ,var)))
		 ((> ,var ,gstop)))))
;;; 正しく動作するもの
(defun ntha (n lst)
  (if (= n 0)
	  (car lst)
	  (ntha (- n 1) (cdr lst))))
;;; コンパイルできないもの
(defmacro nthb (n lst)
  `(if (= ,n 0)
	   (car ,lst)
	   (nthb (- ,n 1) (cdr ,lst))))

(defmacro nthc (n lst)
  `(do ((n2 ,n (1- n2))
		(lst2 ,lst (cdr lst2)))
	   ((= n2 0) (car lst2))))

(defmacro nthd (n lst)
  `(nth-fn ,n ,lst))

(defun nth-fn (n lst)
  (if (= n 0)
	  (car lst)
	  (nth-fn (- n 1)
			  (cdr lst))))

(defmacro nthe (n lst)
  `(labels ((nth-fn (n lst)
			  (if (= n 0)
				  (car lst)
				  (nth-fn (- n 1) (cdr lst)))))
	 (nth-fn ,n ,lst)))
(defmacro ora (&rest args)
  (or-expand args))

(defun or-expand (args)
  (if (null args)
	  nil
	  (let ((sym (gensym)))
		`(let ((,sym ,(car args)))
		   (if ,sym
			   ,sym
			   ,(or-expand (cdr args)))))))

(defmacro orb (&rest args)
  (if (null args)
	  nil
	  (let ((sym (gensym)))
		`(let ((,sym ,(car args)))
		   (if ,sym
			   ,sym
			   (orb ,@(cdr args)))))))

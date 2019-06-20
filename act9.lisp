;;; 補足を起こしやすい
(defmacro before (x y seq)
  `(let ((seq ,seq))
	 (< (position ,x seq)
		(position ,y seq))))
;;; 正しいバージョン
(defmacro before (x y seq)
  `(let ((xval ,x) (yval ,y) (seq ,seq))
	 (< (position xval seq)
		(position yval seq))))
;;; 補足を起こしやすい
(defmacro for ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var))
		(limit ,stop))
	   ((> ,var limit))
	 ,@body))
;;; 正しいバージョン
(defmacro for ((var start stop) &body body)
  `(do ((b #'(lambda (,var) ,@body))
		(count ,start (1+ count)))
	   ((> count limit))
	 (funcall b count)))
;;; 正しいバージョン2
(defmacro for ((var start stop &body body))
  (let ((gstop (gensym)))
	`(do ((,var ,start (1+ ,var))
		  (,gstop ,stop))
		 ((> ,var ,gstop))
	   ,@body)))

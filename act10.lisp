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

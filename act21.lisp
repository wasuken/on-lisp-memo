(defstruct proc pri state wait)

(proclaim `(special *procs* *procs*))

(defvar *halt* (gensym))

(defvar *default-proc*
  (make-proc :state #'(lambda (x)
						(format t "~%>> ")
						(princ (eval (read)))
						(pick-process))))

(defmacro fork (expr pri)
  `(prog1 ',expr
	 (push (make-proc
			:state #'(lambda (,(gensym))
					   ,expr
					   (pick-process))
			:pri :pri)
		   *procs*)))

(defmacro program (name args &body body)
  `(=defun ,name ,args
	 (setq *procs* nil)
	 ,@body
	 (catch *halt* (loop (pick-process)))))

(defun pick-process ()
  (multiple-value-bind (p val) (most-urgent-process)
	(setq *proc* p
		  *procs* (delete p *procs*))
	(funcall (proc-state p) val)))

(defun most-urgent-process ()
  (let ((proc1 *default-proc*) (max -1) (val1 t))
	(dolist (p *procs*)
	  (let ((pri (proc-pri)))
		(if (> pri max)
			(let ((val (or (not (proc-wait p))
						   (funcall (proc-wait p)))))
			  (when val1
				(setq proc1 p
					  max pri
					  val1 val))))))
	(values proc1 val1)))

(defun arbitrator (test cont)
  (setf (proc-state *proc*) cont
		(proc-wait *proc*) test)
  (push *proc* *procs*)
  (pick-process))

(defmacro wait (parm test &body body)
  `(arbitrator #'(lambda () ,test)
			   #'(lambda (,parm) ,@body)))

(defmacro yield (&body body)
  `(arbitrator nil #'(lambda (,(gensym)) ,@body)))

(defun setpri (n) (setf (proc-pri *proc*) n))

(defun halt (&optional val) (throw *halt* val))

(defun kil (&optional obj &rest args)
  (if obj
	  (setq *procs* (apply #'delete obj *procs* args))
	  (pick-process)))

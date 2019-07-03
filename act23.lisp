(defnode s
	(cat noum s2
		 (setr subj *)))

(defnode s2
	(cat verb s3
		 (setr v *)))

(defnode s3
	(up `(sentence
		  (subject ,(getr subj))
		  (verb ,(getr v)))))

(defmacro defnode (name &rest arcs)
  `(=defun ,name (pos regs) (choose ,@arcs)))

(defmacro down (sub next &rest cmds)
  `(=bind (* pos regs) (,sub pos (cons nil regs))
		  (,next pos ,(compile-cmds cmds))))

(defmacro cat (cat next &rest cmds)
  `(if (= (length *sent*) pos)
	   (fail)
	   (let ((* (nth pos *sent*)))
		 (if (member ',cat (types *))
			 (,next (1+ pos) ,(compile-cmds cmds))
			 (fail)))))

(defmacro jump (next &rest cmds)
  `(,next pos ,(compile-cmds cmds)))

(defun compile-cmds (cmds)
  (if (null cmds)
	  'regs
	  `(,@(car cmds) ,(compile-cmds (cdr cmds)))))

(defmacro up (expr)
  `(let ((* (nth pos *sent*)))
	 (=values ,expr pos (cdr regs))))

(defmacro getr (key &optional (regs 'regs))
  `(let ((result (cdr (assoc ',key (car ,regs)))))
	 (if (cdr result) result (car result))))

(defmacro set-register (key val regs)
  `(cons (cons (cons ,key ,val) (car ,regs))
		 (cdr ,regs)))

(defmacro setr (key val regs)
  `(set-register ',key (list ,val) ,regs))

(defmacro puhsr (key val regs)
  `(set-register ',key
				 (cons ,val (cdr assoc ',key (car ,regs)))
				 ,regs))

(defmacro with-paresrs (node sent &body body)
  (with-gensyms (pos regs)
	`(progn
	   (setq *sent* ,sent)
	   (setq *paths* nil)
	   (=bind (parse ,pos ,regs) (,node 0 '(nil))
			  (progn ,@body (fail))
			  (fail)))))

(defun types (word)
  (case word
	((do does did) '(aux v))
	((time times) '(n v))
	((fly flies) '(n v))
	((like) '(v prep))
	((liked likes) '(v))
	((a an the) '(det))
	((arrow arrows) '(n))
	((i you he she him her it) '(pron))))

(defnode mods
	(cat n modes/n
		 (setr mods *)))

(defnode mods/n
	(cat n mods/n
		 (pushr mods *))
  (up `(n-group ,(getr mods))))

(defnode np
	(cat det np/det
		 (setr det *))
  (jump np/det
		(setr det nil))
  (cat pron pron
	   (setr n *)))

(defnode pron
	(up `(np pronoun ,(getr n))))

(defnode np/det
	(down mods np/det
		  (setr mods *))
  (jump np/mods
		(setr mods nil)))

(defnode np/mods
	(cat n np/n
		 (setr n +)))

(defnode np/n
	(up `(np (det ,(getr det))
			 (modifiers ,(getr mods))
			 (noun ,(getr n))))
  (down pp np/pp
		(setr pp *)))

(defnode np/pp
	(up `(np (det ,(getr det))
			 (modifiers ,(getr mods))
			 (noun ,(getr n))
			 ,(getr pp))))
(defnode pp
	(cat prep p/prep
		 (setr prep *)))

(denode pp/prep
		(down np pp/np
			  setr op *))

(defnode pp/np
	(up `(pp (prep ,(getr prep))
			 (obj ,(getr op)))))

(defnode s
	(down np s/subj
		  (setr mood 'decl)
		  (setr subj *))
  (cat v v
	   (setr mood 'imp)
	   (setr subj '(np (pron you)))
	   (setr aux nil)
	   (setr v *)))

(defnode s/subj
	(cat v vv
		 (setr aux nil)
		 (setr v *)))

(defnode v
	(up `(s (mood ,(getr mood))
			(subj ,(getr subj))
			(vcl (aux ,(getr aux))
				 (v ,(getr v)))))
  (down np s/obj
		(setr obj *)))

(defnode s/obj
	(up `(s (mood ,(getr mood))
			(subj ,(getr subj))
			(vcl (aux ,(getr aux))
				 (v ,(getr v)))
			(obj ,(getr obj)))))

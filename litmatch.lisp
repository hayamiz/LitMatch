
(defpackage #:litmatch
  (:nicknames #:lm)
  (:use :common-lisp)
  (:export #:litmatch
		   ;; #:extract-vars
		   ))

(in-package :litmatch)

(defun extract-vars (matcher &optional init-vars)
  (cond
	((symbolp matcher)
	 (let ((var (symbol-name matcher)))
	   (when (and (> (length var) 1)
				(string-equal "v%" (subseq var 0 2)))
		 (union init-vars (list (intern (subseq var 2) *package*))))))
	((consp matcher)
	 (union init-vars (union (extract-vars (car matcher))
							 (extract-vars (cdr matcher)))))))

(defmacro litmatch (value &rest clauses)
  (let ((vars nil)
		(partial-match-f (gensym)))
	(dolist (clause clauses)
	  (setq vars (extract-vars (car clause) vars)))
	`(let ,vars
	   (labels ((,partial-match-f (p-val p-mat)
				  (cond
					,@(mapcar (lambda (var) `((eq ',(intern (concatenate 'string "V%" (symbol-name var))
															*package*)
												  p-mat)
											  (setf ,var p-val)
											  t))
							  vars)
					((eq ',(intern '_ *package*) p-mat) t)
					((and (consp p-val) (consp p-mat))
					 (and (,partial-match-f (car p-val) (car p-mat))
						  (,partial-match-f (cdr p-val) (cdr p-mat))))
					(t (equal p-mat p-val)))))
	   (cond
		 ,@(mapcar (lambda (clause)
					 (let ((matcher (car clause))
						   (body    (cadr clause)))
					   (if (eq matcher t)
						   `(t ,body)
						   `((,partial-match-f ,value (quote ,matcher))
							 ,body))))
				   clauses))))))

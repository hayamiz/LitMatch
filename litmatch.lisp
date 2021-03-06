;;; LitMatch -- literal pattern matcher for common lisp

;; LitMatch is licensed under the MIT lincense

;; Copyright (c) 2010, Yuto HAYAMIZU <y.hayamizu@gmail.com>

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Introduction

;; LitMatch is a pattern matching library. It is very useful for
;; switching control flow depending on complex data structure. For
;; example, suppose that a variable "x" may hold '(a b (c d)) or '(a b
;; (1 2)) and you want to distinguish these cases. You can write as follows:

;;     (litmatch:litmatch x
;;       ((a b (c d))  "this is a former case")
;;       ((a b (1 2))  "this is a latter case"))


(defpackage #:litmatch
  (:nicknames #:lm)
  (:use :common-lisp)
  (:export #:litmatch
		   #:extract-vars
		   ))

(in-package :litmatch)

(defun extract-vars (matcher &optional init-vars)
  (union init-vars
		 (cond
		   ((symbolp matcher)
			(let ((var (symbol-name matcher)))
			  (cond
				((and (> (length var) 1)
					  (string-equal "v%" (subseq var 0 2)))
				 (list (intern (subseq var 2) *package*)))
				((and (> (length var) 2)
					  (string-equal "as%" (subseq var 0 3)))
				 (list (intern (subseq var 3) *package*))))))
		   ((consp matcher)
			(union (extract-vars (car matcher))
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
					,@(mapcar (lambda (var) `((and (consp p-mat)
												   (eq ',(intern (concatenate 'string "AS%" (symbol-name var))
																 *package*)
													   (cdr p-mat)))
											  (setf ,var p-val)
											  (,partial-match-f p-val (car p-mat))))
							  vars)
					((and (listp p-mat) (eq ',(intern '%or *package*) (car p-mat))
						  (not (null (cdr p-mat))))
					 (some (lambda (p-mat)
							 (,partial-match-f p-val p-mat))
						   (cdr p-mat)))
					((eq ',(intern '_ *package*) p-mat) t)
					((and (consp p-val) (consp p-mat))
					 (and (,partial-match-f (car p-val) (car p-mat))
						  (,partial-match-f (cdr p-val) (cdr p-mat))))
					(t (equal p-mat p-val)))))
	   (cond
		 ,@(mapcar (lambda (clause)
					 (let ((matcher (car clause))
						   (body    (cdr clause)))
					   (if (eq matcher t)
						   `(t (progn ,@body))
						   `((,partial-match-f ,value (quote ,matcher))
							 (progn ,@body)))))
				   clauses))))))

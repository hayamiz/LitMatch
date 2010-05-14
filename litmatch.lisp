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

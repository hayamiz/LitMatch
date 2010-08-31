
(pushnew (merge-pathnames ".asdf-install-dir/systems/"
						  (user-homedir-pathname))
		 asdf:*central-registry*
		 :test #'equal)

(require :litmatch
		 (if *load-pathname*
			 (merge-pathnames
			  "litmatch.lisp"
			  (merge-pathnames
			   ".." (make-pathname :directory (pathname-directory *load-pathname*))))
			 nil))
(asdf:operate 'asdf:load-op :fiveam)

(use-package :fiveam)
(use-package :litmatch)

(def-suite litmatch-test :description "test suite for lexer")
(in-suite litmatch-test)

(test test-num
  (is (eq 'ok
		  (litmatch:litmatch 1
			(1 'ok))))
  (is (eq 'ok
		  (litmatch:litmatch 2
			(1 'ng)
			(2 'ok)
			(3 'ng))))
  (is (eq 'ok
		  (litmatch:litmatch 4
			(1 'ng)
			(2 'ng)
			(3 'ng)
			(t 'ok))))
  )

(test test-symbol
  (is (eq 'ok
		  (litmatch:litmatch 'foo
			(foo 'ok))))
  (is (eq 'ok
		  (litmatch:litmatch 'bar
			(foo 'ng)
			(bar 'ok))))
  (is (eq 'ok
		  (litmatch:litmatch 'baz
			(foo 'ng)
			(bar 'ng)
			(t 'ok)))))

(test test-any
  (is (eq 'ok
		  (litmatch:litmatch '(1 2 (3 foo))
			((1 2 (3 . _)) 'ok))))
  (is (eq 'ok
		  (litmatch:litmatch '(1 2 (3 foo))
			(_ 'ok)))))

(test test-bind
  (is (equal (list 3 4)
			 (litmatch:litmatch '(1 2 (3 4))
			   ((1 2 v%foo) foo))))
  (is (equal (list '(2 (3 4)) '(3 4))
			 (litmatch:litmatch '(1 2 (3 4))
			   ((1 . ((2 v%part) . as%whole))
				(list whole part))))))

(test test-or
  (is (eq 'ok
		  (litmatch:litmatch '(1 2 3 4 5)
			((1 2 3 . (%or nil (4) (4 5))) 'ok))))
  (is (eq 'ok
		  (litmatch:litmatch '(1 2 3 4)
			((1 2 3 . (%or nil (4) (4 5))) 'ok))))
  (is (eq 'ok
		  (litmatch:litmatch '(1 2 3)
			((1 2 3 . (%or nil (4) (4 5))) 'ok)))))

(test test-multi-body
  (is (eq 'ok
		  (litmatch:litmatch '(1 (2 3))
			((1 (2 3))
			 'boom
			 'ok))))
  (is (eq 'ok
		  (litmatch:litmatch '(1 (2 3))
			((1 2 3)
			 'boom
			 'ng)
			((1 (2 3))
			 'booom
			 'ok)))))

(mapc 'run! '(litmatch-test))

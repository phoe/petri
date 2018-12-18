;;;; test.lisp

(defpackage #:petri/tests
  (:use #:cl
        #:alexandria
        #:petri
        #:1am))

(in-package #:petri/tests)

(defun %test (petri-net input-bag input output-bag output)
  (map nil (curry #'bag-insert (bag petri-net input-bag)) input)
  (funcall petri-net)
  (let ((contents (bag-contents (bag petri-net output-bag))))
    (is (set-equal (coerce output 'list) (coerce contents 'list)))))

;;; TEST 1

(defun %test-1-callback (input output)
  (setf (gethash 'bar output)
        (mapcar #'- (gethash 'foo input))))

(test test-petri-net-1-1
  (%test (make-petri-net '(foo bar)
           (list (make-transition 'foo 'bar #'%test-1-callback)))
         'foo #(0 1 2 3 4 5 6 7 8 9)
         'bar #(0 -1 -2 -3 -4 -5 -6 -7 -8 -9)))

(test test-petri-net-1-2
  (%test (petri-net ()
           (foo -> #'%test-1-callback)
           (#'%test-1-callback -> bar))
         'foo #(0 1 2 3 4 5 6 7 8 9)
         'bar #(0 -1 -2 -3 -4 -5 -6 -7 -8 -9)))

;;; TEST 2

(defun %test-2-callback-1 (input output)
  (setf (gethash 'bar output) (mapcar #'- (gethash 'foo input))
        (gethash 'baz output) (gethash 'foo input)))

(defun %test-2-callback-2 (input output)
  (setf (gethash 'quux output)
        (append (gethash 'baz input) (gethash 'bar input))))

(test test-petri-net-2-1
  (%test (make-petri-net '(foo bar baz quux)
           (list (make-transition 'foo '(bar baz) #'%test-2-callback-1)
                 (make-transition '(bar baz) 'quux #'%test-2-callback-2)))
         'foo #(1 2 3)
         'quux #(-3 -2 -1 1 2 3)))

(test test-petri-net-2-2
  (%test (petri-net ()
           (foo -> #'%test-2-callback-1 -> bar baz)
           (bar baz -> #'%test-2-callback-2 -> quux))
         'foo #(1 2 3)
         'quux #(-3 -2 -1 1 2 3)))

;;; TEST 3

(defun %test-3-callback (input output)
  (setf (gethash 'bar output)
        (list (reduce #'+ (gethash 'foo input)))))

(test test-petri-net-3-1
  (%test (make-petri-net '(foo bar)
           (list (make-transition '((foo 3)) 'bar #'%test-3-callback)))
         'foo #(1 1 1 1 1 1 1 1 1)
         'bar #(3 3 3)))

(test test-petri-net-3-2
  (%test (petri-net ()
           ((foo 3) -> #'%test-3-callback)
           (#'%test-3-callback -> bar))
         'foo #(1 1 1 1 1 1 1 1 1)
         'bar #(3 3 3)))

;;; TEST 4

(defun %test-4-callback (input output)
  (dolist (value (gethash 'foo input))
    (dotimes (i 3)
      (push (/ value 3) (gethash 'bar output)))))

(test test-petri-net-4-1
  (%test (make-petri-net '(foo bar)
           (list (make-transition 'foo 'bar #'%test-4-callback)))
         'foo #(3 3 3)
         'bar #(1 1 1 1 1 1 1 1 1)))

(test test-petri-net-4-2
  (%test (petri-net ()
           (foo -> #'%test-4-callback)
           (#'%test-4-callback -> bar))
         'foo #(3 3 3)
         'bar #(1 1 1 1 1 1 1 1 1)))

;;; NEGATIVE TEST 1

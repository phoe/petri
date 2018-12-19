;;;; test.lisp

(defpackage #:petri/test
  (:use #:cl
        #:alexandria
        #:petri
        #:1am)
  (:shadow #:test #:run)
  (:export #:run))

(in-package #:petri/test)

(defvar *petri-tests* '())

(defun run ()
  (1am:run *petri-tests*))

(defmacro define-test (name &body body)
  `(let ((1am:*tests* '()))
     (1am:test ,name ,@body)
     (pushnew ',name *petri-tests*)))

(defun %test (petri-net input-bag input output-bag output)
  (map nil (curry #'bag-insert (bag petri-net input-bag)) input)
  (funcall petri-net)
  (let ((contents (bag-contents (bag petri-net output-bag))))
    (is (set-equal (coerce output 'list) (coerce contents 'list)))))

;;; TEST 1

(defun %test-1-callback (input output)
  (setf (gethash 'bar output)
        (mapcar #'- (gethash 'foo input))))

(define-test test-petri-net-1-1
  (%test (make-petri-net '(foo bar)
           (list (make-transition 'foo 'bar #'%test-1-callback)))
         'foo #(0 1 2 3 4 5 6 7 8 9)
         'bar #(0 -1 -2 -3 -4 -5 -6 -7 -8 -9)))

(define-test test-petri-net-1-2
  (%test (petri-net ()
           (foo -> #'%test-1-callback -> bar))
         'foo #(0 1 2 3 4 5 6 7 8 9)
         'bar #(0 -1 -2 -3 -4 -5 -6 -7 -8 -9)))

;;; TEST 2

(defun %test-2-callback-1 (input output)
  (setf (gethash 'bar output) (mapcar #'- (gethash 'foo input))
        (gethash 'baz output) (gethash 'foo input)))

(defun %test-2-callback-2 (input output)
  (setf (gethash 'quux output)
        (append (gethash 'baz input) (gethash 'bar input))))

(define-test test-petri-net-2-1
  (%test (make-petri-net '(foo bar baz quux)
           (list (make-transition 'foo '(bar baz) #'%test-2-callback-1)
                 (make-transition '(bar baz) 'quux #'%test-2-callback-2)))
         'foo #(1 2 3)
         'quux #(-3 -2 -1 1 2 3)))

(define-test test-petri-net-2-2
  (%test (petri-net ()
           (foo -> #'%test-2-callback-1 -> bar baz
                -> #'%test-2-callback-2 -> quux))
         'foo #(1 2 3)
         'quux #(-3 -2 -1 1 2 3)))

;;; TEST 3

(defun %test-3-callback (input output)
  (setf (gethash 'bar output)
        (list (reduce #'+ (gethash 'foo input)))))

(define-test test-petri-net-3-1
  (%test (make-petri-net '(foo bar)
           (list (make-transition '((foo 3)) 'bar #'%test-3-callback)))
         'foo #(1 1 1 1 1 1 1 1 1)
         'bar #(3 3 3)))

(define-test test-petri-net-3-2
  (%test (petri-net ()
           ((foo 3) -> #'%test-3-callback -> bar))
         'foo #(1 1 1 1 1 1 1 1 1)
         'bar #(3 3 3)))

;;; TEST 4

(defun %test-4-callback (input output)
  (dolist (value (gethash 'foo input))
    (dotimes (i 3)
      (push (/ value 3) (gethash 'bar output)))))

(define-test test-petri-net-4-1
  (%test (make-petri-net '(foo bar)
           (list (make-transition 'foo 'bar #'%test-4-callback)))
         'foo #(3 3 3)
         'bar #(1 1 1 1 1 1 1 1 1)))

(define-test test-petri-net-4-2
  (%test (petri-net ()
           (foo -> #'%test-4-callback  -> bar))
         'foo #(3 3 3)
         'bar #(1 1 1 1 1 1 1 1 1)))

;;; NEGATIVE TEST - MISMATCHED OUTPUT

(defun %test-negative-mismatched-callback (input output)
  (declare (ignore input))
  (setf (gethash 'bar output) (list 42)
        (gethash 'baz output) (list 42)))

(define-test test-negative-mismatched-1
  (signals petri-net-error
    (%test (make-petri-net '(foo bar)
             (list (make-transition 'foo 'bar
                                    #'%test-negative-mismatched-callback)))
           'foo #(42)
           'bar #(42))))

(define-test test-negative-mismatched-2
  (signals petri-net-error
    (%test (petri-net ()
             (foo -> #'%test-negative-mismatched-callback -> bar))
           'foo #(42)
           'bar #(42))))

;;; NEGATIVE TEST - INVALID KEY

(defun %test-negative-invalid-key-callback (input output)
  (declare (ignore input))
  (setf (gethash 42 output) (list 42)))

(define-test test-negative-invalid-key-1
  (signals petri-net-error
    (%test (make-petri-net '(foo bar)
             (list (make-transition 'foo 'bar
                                    #'%test-negative-invalid-key-callback)))
           'foo #(42)
           'bar #(42))))

(define-test test-negative-invalid-key-2
  (signals petri-net-error
    (%test (petri-net ()
             (foo -> #'%test-negative-invalid-key-callback -> bar))
           'foo #(42)
           'bar #(42))))

;;; NEGATIVE TEST - INVALID VALUE

(defun %test-negative-invalid-value-callback (input output)
  (declare (ignore input))
  (setf (gethash 'bar output) 42))

(define-test test-negative-invalid-value-1
  (signals petri-net-error
    (%test (make-petri-net '(foo bar)
             (list (make-transition 'foo 'bar
                                    #'%test-negative-invalid-value-callback)))
           'foo #(42)
           'bar #(42))))

(define-test test-negative-invalid-value-2
  (signals petri-net-error
    (%test (petri-net ()
             (foo -> #'%test-negative-invalid-value-callback -> bar))
           'foo #(42)
           'bar #(42))))

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

(defun make-test (input-bag input output-bag output)
  (lambda (petri-net)
    (map nil (curry #'bag-insert (bag petri-net input-bag)) input)
    (funcall petri-net)
    (let ((contents (bag-contents (bag petri-net output-bag))))
      (is (set-equal (coerce output 'list) (coerce contents 'list))))))

;;; TEST 1

(defun %test-1-callback (input output)
  (setf (gethash 'bar output)
        (mapcar #'- (gethash 'foo input))))

(define-test test-petri-net-1
  (let ((test (make-test 'foo #(0 1 2 3 4 5 6 7 8 9)
                         'bar #(0 -1 -2 -3 -4 -5 -6 -7 -8 -9)))
        (args `((foo bar) ((foo bar ,#'%test-1-callback)))))
    (funcall test (apply #'make-petri-net args))
    (funcall test (apply #'make-threaded-petri-net args))
    (funcall test (petri-net () (foo -> #'%test-1-callback -> bar)))
    (funcall test (threaded-petri-net () (foo -> #'%test-1-callback -> bar)))))

;;; TEST 2

(defun %test-2-callback-1 (input output)
  (setf (gethash 'bar output) (mapcar #'- (gethash 'foo input))
        (gethash 'baz output) (gethash 'foo input)))

(defun %test-2-callback-2 (input output)
  (setf (gethash 'quux output)
        (append (gethash 'baz input) (gethash 'bar input))))

(define-test test-petri-net-2
  (let ((test (make-test 'foo #(1 2 3)
                         'quux #(-3 -2 -1 1 2 3)))
        (args `((foo bar baz quux)
                ((foo (bar baz) ,#'%test-2-callback-1)
                 ((bar baz) quux ,#'%test-2-callback-2)))))
    (funcall test (apply #'make-petri-net args))
    (funcall test (apply #'make-threaded-petri-net args))
    (funcall test (petri-net ()
                    (foo -> #'%test-2-callback-1 -> bar baz
                         -> #'%test-2-callback-2 -> quux)))
    (funcall test (threaded-petri-net ()
                    (foo -> #'%test-2-callback-1 -> bar baz
                         -> #'%test-2-callback-2 -> quux)))))

;;; TEST 3

(defun %test-3-callback (input output)
  (setf (gethash 'bar output)
        (list (reduce #'+ (gethash 'foo input)))))

(define-test test-petri-net-3
  (let ((test (make-test 'foo #(1 1 1 1 1 1 1 1 1)
                         'bar #(3 3 3)))
        (args `((foo bar) ((((foo 3)) bar ,#'%test-3-callback)))))
    (funcall test (apply #'make-petri-net args))
    (funcall test (apply #'make-threaded-petri-net args))
    (funcall test (petri-net ()
                    ((foo 3) -> #'%test-3-callback -> bar)))
    (funcall test (threaded-petri-net ()
                    ((foo 3) -> #'%test-3-callback -> bar)))))

;;; TEST 4

(defun %test-4-callback (input output)
  (dolist (value (gethash 'foo input))
    (dotimes (i 3)
      (push (/ value 3) (gethash 'bar output)))))

(define-test test-petri-net-4
  (let ((test (make-test 'foo #(3 3 3)
                         'bar #(1 1 1 1 1 1 1 1 1)))
        (args `((foo bar) ((foo bar ,#'%test-4-callback)))))
    (funcall test (apply #'make-petri-net args))
    (funcall test (apply #'make-threaded-petri-net args))
    (funcall test (petri-net ()
                    ((foo 3) -> #'%test-4-callback -> bar)))
    (funcall test (threaded-petri-net ()
                    ((foo 3) -> #'%test-4-callback -> bar)))))

;;; NEGATIVE TEST - MISMATCHED OUTPUT

(defun %test-negative-mismatched-callback (input output)
  (declare (ignore input))
  (setf (gethash 'bar output) (list 42)
        (gethash 'baz output) (list 42)))

(define-test test-negative-mismatched
  (let ((test (make-test 'foo #(3 3 3)
                         'bar #(1 1 1 1 1 1 1 1 1)))
        (args `((foo bar) ((foo bar ,#'%test-negative-mismatched-callback)))))
    (signals petri-net-error
      (funcall test (apply #'make-petri-net args)))
    (signals petri-net-error
      (funcall test (apply #'make-threaded-petri-net args)))
    (signals petri-net-error
      (funcall test (petri-net ()
                      (foo -> #'%test-negative-mismatched-callback -> bar))))
    (signals petri-net-error
      (funcall test (threaded-petri-net ()
                      (foo -> #'%test-negative-mismatched-callback -> bar))))))

;;; NEGATIVE TEST - INVALID KEY

(defun %test-negative-invalid-key-callback (input output)
  (declare (ignore input))
  (setf (gethash 42 output) (list 42)))

(define-test test-negative-invalid-key
  (let ((test (make-test 'foo #(42)
                         'bar #(42)))
        (args `((foo bar) ((foo bar ,#'%test-negative-invalid-key-callback)))))
    (signals petri-net-error
      (funcall test (apply #'make-petri-net args)))
    (signals petri-net-error
      (funcall test (apply #'make-threaded-petri-net args)))
    (signals petri-net-error
      (funcall test (petri-net ()
                      (foo -> #'%test-negative-invalid-key-callback -> bar))))
    (signals petri-net-error
      (funcall test (threaded-petri-net ()
                      (foo -> #'%test-negative-invalid-key-callback -> bar))))))

;;; NEGATIVE TEST - INVALID VALUE

(defun %test-negative-invalid-value-callback (input output)
  (declare (ignore input))
  (setf (gethash 'bar output) 42))

(define-test test-negative-invalid-value
  (let ((test (make-test 'foo #(42)
                         'bar #(42)))
        (args `((foo bar) ((foo bar ,#'%test-negative-invalid-value-callback)))))
    (signals petri-net-error
      (funcall test (apply #'make-petri-net args)))
    (signals petri-net-error
      (funcall test (apply #'make-threaded-petri-net args)))
    (signals petri-net-error
      (funcall test
               (petri-net ()
                 (foo -> #'%test-negative-invalid-value-callback -> bar))))
    (signals petri-net-error
      (funcall test
               (threaded-petri-net ()
                 (foo -> #'%test-negative-invalid-value-callback -> bar))))))

;;; TEST-TIME
;;; This verifies that the threaded net is indeed paralellizing execution.
;;; Uncomment when needed, manually compare TIME results.

;; (defun %test-time-callback (input output)
;;   (sleep 0.1)
;;   (setf (gethash 'bar output)
;;         (mapcar #'- (gethash 'foo input))))

;; (define-test test-time
;;   (let ((test (make-test 'foo #(0 1 2 3 4 5 6 7 8 9)
;;                          'bar #(0 -1 -2 -3 -4 -5 -6 -7 -8 -9)))
;;         (args `((foo bar) ((foo bar ,#'%test-time-callback)))))
;;     (time (funcall test (apply #'make-petri-net args)))
;;     (time (funcall test (apply #'make-threaded-petri-net args)))))

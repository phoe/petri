;;;; test.lisp

(defpackage #:petri/test
  (:use #:cl
        #:alexandria
        #:petri
        #:petri/threaded
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

(defun make-test (input-bag input output-bag output
                  &key extra-input-bag extra-input)
  (lambda (petri-net)
    (map nil (curry #'bag-insert (bag-of petri-net input-bag)) input)
    (when extra-input
      (map nil (curry #'bag-insert (bag-of petri-net extra-input-bag))
           extra-input))
    (is (eq petri-net (funcall petri-net)))
    (let ((contents (bag-contents (bag-of petri-net output-bag))))
      (is (set-equal (coerce output 'list) (coerce contents 'list))))))

;;; TEST 1

(defun %test-1-callback (input output)
  (setf (gethash 'bar output) (mapcar #'- (gethash 'foo input))))

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
                 ((bar baz) ((quux 2)) ,#'%test-2-callback-2)))))
    (funcall test (apply #'make-petri-net args))
    (funcall test (apply #'make-threaded-petri-net args))
    (funcall test (petri-net ()
                    (foo -> #'%test-2-callback-1 -> bar baz
                         -> #'%test-2-callback-2 -> (quux 2))))
    (funcall test (threaded-petri-net ()
                    (foo -> #'%test-2-callback-1 -> bar baz
                         -> #'%test-2-callback-2 -> (quux 2))))))

;;; TEST 3

(defun %test-3-callback (input output)
  (push (reduce #'+ (gethash 'foo input))
        (gethash 'bar output)))

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
        (args `((foo bar) ((foo ((bar 3)) ,#'%test-4-callback)))))
    (funcall test (apply #'make-petri-net args))
    (funcall test (apply #'make-threaded-petri-net args))
    (funcall test (petri-net ()
                    (foo -> #'%test-4-callback -> (bar 3))))
    (funcall test (threaded-petri-net ()
                    (foo -> #'%test-4-callback -> (bar 3))))))

(define-test test-petri-net-4-wildcard
  (let ((test (make-test 'foo #(3 3 3)
                         'bar #(1 1 1 1 1 1 1 1 1)))
        (args `((foo bar) ((foo ((bar *)) ,#'%test-4-callback)))))
    (funcall test (apply #'make-petri-net args))
    (funcall test (apply #'make-threaded-petri-net args))
    (funcall test (petri-net ()
                    (foo -> #'%test-4-callback -> (bar *))))
    (funcall test (threaded-petri-net ()
                    (foo -> #'%test-4-callback -> (bar *))))))

;;; TEST 5

(defun %test-5-callback (input output)
  (declare (ignore input))
  (push 42 (gethash 'baz output)))

(define-test test-petri-net-5
  (let ((test (make-test 'foo #(1 2 3 4 5)
                         'baz #(42 42 42 42 42)))
        (args `((foo bar baz) (((foo (bar !)) baz ,#'%test-5-callback)))))
    (funcall test (apply #'make-petri-net args))
    (funcall test (apply #'make-threaded-petri-net args))
    (funcall test (petri-net ()
                    (foo (bar !) -> #'%test-5-callback -> baz)))
    (funcall test (threaded-petri-net ()
                    (foo (bar !) -> #'%test-5-callback -> baz)))))

(define-test test-petri-net-5-inhibitor-full
  (let ((test (make-test 'foo #(1 2 3 4 5)
                         'baz #()
                         :extra-input-bag 'bar
                         :extra-input #(1)))
        (args `((foo bar baz) (((foo (bar !)) baz ,#'%test-5-callback)))))
    (funcall test (apply #'make-petri-net args))
    (funcall test (apply #'make-threaded-petri-net args))
    (funcall test (petri-net ()
                    (foo (bar !) -> #'%test-5-callback -> baz)))
    (funcall test (threaded-petri-net ()
                    (foo (bar !) -> #'%test-5-callback -> baz)))))

;;; NEGATIVE TEST - MISMATCHED OUTPUT

(defun %test-negative-mismatched-callback (input output)
  (declare (ignore input))
  (push 42 (gethash 'bar output))
  (push 42 (gethash 'baz output)))

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
  (push 42 (gethash 42 output)))

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

;;; NEGATIVE TEST - INVALID MACRO FORMS

(defun %test-invalid-form (data)
  (signals petri-net-error (macroexpand `(petri-net () ,@data))))

(define-test test-negative-invalid-macro-form
  (%test-invalid-form '(42))
  (%test-invalid-form '((foo bar baz)))
  (%test-invalid-form '((#'foo #'bar #'baz)))
  (%test-invalid-form '((foo -> bar)))
  (%test-invalid-form '((#'foo -> #'bar)))
  (%test-invalid-form '((42 -> foo)))
  (%test-invalid-form '((42 -> #'foo)))
  (%test-invalid-form '((foo -> 42)))
  (%test-invalid-form '((#'foo -> 42))))

(define-test test-negative-invalid-wildcard-form
  (%test-invalid-form '(((foo *) -> #'bar))))

(define-test test-negative-invalid-inhibitor-form
  (%test-invalid-form '((#'foo -> (bar !)))))

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

;; Define the callback functions for the transition: one which negates its
;; arguments and the other which passes them without any change.
(flet ((callback-negation (input output)
         (push (- (pop (gethash 'foo input))) (gethash 'bar output)))
       (callback-identity (input output)
         (push (pop (gethash 'foo input)) (gethash 'bar output))))
  ;; Create the petri net object.
  (let ((petri-net (make-petri-net
                    ;; Define bags FOO and BAR.
                    '(foo bar)
                    ;; Define two transitions:
                    ;; * one that takes one token from FOO, outputs one token to
                    ;;   BAR, and calls CALLBACK-IDENTITY,
                    ;; * one that takes one token from FOO, outputs one token to
                    ;;   BAR, and calls CALLBACK-NEGATION.
                    `((((foo 1)) ((bar 1)) ,#'callback-negation)
                      (((foo 1)) ((bar 1)) ,#'callback-identity)))))
    ;; Populate bag FOO with data.
    (dolist (i '(1 2 3 4 5 6 7 8 9 0))
      (bag-insert (bag-of petri-net 'foo) i))
    ;; Funcall the Petri net.
    (funcall petri-net)
    ;; Access the contents of bag BAR.
    (bag-contents (bag-of petri-net 'bar))))

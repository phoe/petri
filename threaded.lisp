;;;; threaded.lisp

(uiop:define-package #:petri/threaded
    (:mix #:closer-mop
          #:cl
          #:alexandria
          #:split-sequence
          #:phoe-toolbox/bag
          #:trivial-backtrace
          #:petri)
  (:reexport #:phoe-toolbox/bag)
  (:export ;; ASYNC
           #:threaded-petri-net
           #:make-threaded-petri-net
           #:threaded-petri-net-error))

(in-package #:petri/threaded)

;;; THREADED

(defclass threaded-petri-net (petri-net)
  ((%lock :reader lock-of
          :initform (bt:make-lock))
   (%thread-queue :reader thread-queue
                  :initform (lparallel.queue:make-queue)))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod petri::petri-net-transition-constructor
    ((petri-net threaded-petri-net))
  #'make-threaded-transition)

(defmethod petri::make-petri-net-funcallable-function
    ((petri-net threaded-petri-net))
  (named-lambda execute-threaded-petri-net
      (&optional (compress t) ignore-errors)
    (bt:with-lock-held ((lock-of petri-net))
      (spawn-transitions petri-net))
    (let ((errorp (join-all-threads petri-net ignore-errors)))
      (when compress
        (dolist (bag (hash-table-values (petri::bags petri-net)))
          (bag-compress bag)))
      (values petri-net errorp))))

(defun spawn-transitions (petri-net)
  (flet ((spawn ()
           (when-let ((transition (petri::find-ready-transition petri-net)))
             (let ((input (petri::populate-input transition petri-net t)))
               (bt:make-thread (curry transition input petri-net))))))
    (loop with queue = (thread-queue petri-net)
          for thread = (spawn)
          while thread do (lparallel.queue:push-queue thread queue))))

(defun join-all-threads (petri-net ignore-errors)
  (loop with errorp = nil
        with queue = (thread-queue petri-net)
        for thread = (lparallel.queue:try-pop-queue queue)
        while thread
        do (multiple-value-bind (condition backtrace) (bt:join-thread thread)
             (cond ((not (typep condition 'condition)))
                   (ignore-errors (setf errorp t))
                   (t (threaded-petri-net-error condition backtrace))))
        finally (return errorp)))

(defclass threaded-transition (petri::transition) ()
  (:metaclass closer-mop:funcallable-standard-class))

(defmacro with-threaded-petri-net-handler ((condition backtrace) &body body)
  (with-gensyms (e)
    `(block nil
       (handler-bind
           ((error (lambda (,e)
                     (setf ,condition ,e
                           ,backtrace (print-backtrace ,e :output nil))
                     (return))))
         ,@body))))

(defmethod petri::make-transition-funcallable-function
    ((transition threaded-transition))
  (named-lambda execute-threaded-transition (input petri-net)
    (let (condition
          backtrace
          (output (petri::make-output-hash-table transition)))
      (with-threaded-petri-net-handler (condition backtrace)
        (petri::call-callback transition input output))
      (unless condition
        (with-threaded-petri-net-handler (condition backtrace)
          (bt:with-lock-held ((lock-of petri-net))
            (petri::populate-output transition petri-net output)
            (spawn-transitions petri-net))))
      (values condition backtrace))))

(defun make-threaded-transition (from to callback)
  (petri::make-transition from to callback 'threaded-transition))

(defun make-threaded-petri-net (bags transitions)
  (make-instance 'threaded-petri-net :bags bags :transitions transitions))

(defmacro threaded-petri-net (() &body forms)
  `(petri::%petri-net #'make-threaded-petri-net ,@forms))

(define-condition threaded-petri-net-error (petri-net-error)
  ((%reason :reader reason
            :initarg :reason
            :initform (required-argument :reason))
   (%backtrace :reader backtrace
               :initarg :backtrace
               :initform nil))
  (:report (lambda (condition stream)
             (format stream "Error while executing the threaded Petri net:~%~A
Backtrace: ~A" (reason condition) (backtrace condition)))))

(defun threaded-petri-net-error (reason backtrace)
  (cerror "Continue executing the Petri net." 'threaded-petri-net-error
          :reason reason :backtrace backtrace))

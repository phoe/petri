;;;; petri.lisp

(uiop:define-package #:petri
  (:mix #:closer-mop
        #:cl
        #:alexandria
        #:split-sequence
        #:phoe-toolbox/bag
        #:trivial-backtrace)
  (:reexport #:phoe-toolbox/bag)
  (:export #:transition #:bags-from #:bags-to #:callback
           #:make-transition
           ;; TODO do we need to explicitly export transitions symbols?
           #:transition-valid-p
           #:petri-net-error #:simple-petri-net-error
           #:transition-not-ready
           #:requested-count
           #:actual-count
           #:condition-bag
           #:petri-net #:bags #:transitions
           #:make-petri-net
           #:find-valid-transitions
           ;; ASYNC
           #:threaded-petri-net
           #:threaded-transition
           #:make-threaded-petri-net))

(in-package #:petri)

;;; BAG

(defun make-bags (&rest symbols)
  (assert (every #'symbolp symbols))
  (alist-hash-table (mapcar (lambda (x) (cons x (make-bag))) symbols)))

;;; PETRI-NET

(defclass petri-net ()
  ((%bags :accessor bags)
   (%transitions :accessor transitions))
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs :bags (make-hash-table) :transitions '()))

(defmethod print-object ((petri-net petri-net) stream)
  (print-unreadable-object (petri-net stream :type t)
    (format stream "(~DP/~DT)"
            (hash-table-count (bags petri-net))
            (length (transitions petri-net)))))

(defmethod initialize-instance :after
    ((petri-net petri-net) &key bags transitions)
  (set-funcallable-instance-function petri-net (petri-net-call petri-net))
  (let ((constructor (petri-net-transition-constructor petri-net)))
    (setf (transitions petri-net)
          (mapcar (curry #'apply constructor) transitions)))
  (setf (slot-value petri-net '%bags)
        (apply #'make-bags (mapcar #'ensure-car bags))))

(defgeneric petri-net-transition-constructor (petri-net))

(defmethod petri-net-transition-constructor ((petri-net petri-net))
  #'make-transition)

(defgeneric petri-net-call (petri-net))

(defmethod petri-net-call ((petri-net petri-net))
  (named-lambda execute-petri-net ()
    (loop for transition = (find-valid-transition petri-net)
          while transition do (funcall transition petri-net t))
    petri-net))

(defgeneric bag (petri-net name)
  (:method ((petri-net petri-net) (name symbol))
    (gethash name (bags petri-net))))

(defgeneric (setf bag) (new-value petri-net name)
  (:method ((new-value symbol) (petri-net petri-net) (name symbol))
    (let ((foundp (nth-value 1 (gethash name (bags petri-net)))))
      (if foundp
          (setf (gethash name (bags petri-net)) new-value)
          (remhash name (bags petri-net)))
      new-value)))

(defun find-valid-transition (petri-net)
  (find-if (rcurry #'transition-valid-p petri-net)
           (shuffle (transitions petri-net))))

(defun make-petri-net (bags transitions)
  (make-instance 'petri-net :bags bags :transitions transitions))

;;; TRANSITION

(defclass transition ()
  ((%bags-from :accessor bags-from
               :initarg :bags-from)
   (%bags-to :accessor bags-to
             :initarg :bags-to)
   (%callback :accessor callback
              :initarg :callback))
  (:default-initargs :bags-from (make-hash-table)
                     :bags-to '()
                     :callback (required-argument :callback))
  (:metaclass closer-mop:funcallable-standard-class))

(defun no-one-element-lists (x)
  (if (and (consp x) (cdr x)) x (car x)))

(defmethod print-object ((transition transition) stream)
  (print-unreadable-object (transition stream :type t)
    (format stream "(~S -> ~S)"
            (no-one-element-lists (hash-table-keys (bags-from transition)))
            (no-one-element-lists (bags-to transition)))))

(defmethod initialize-instance :after ((transition transition) &key)
  (set-funcallable-instance-function transition (transition-call transition)))

;;; TRANSITION VALIDATION

(defun validate-output-hash-table (output transition)
  (let ((alist (hash-table-alist output)))
    (dolist (entry alist)
      (destructuring-bind (key . value) entry
        (unless (symbolp key)
          (petri-net-error "Key ~S in the output table is not a symbol." key))
        (unless (proper-list-p value)
          (petri-net-error "Value ~S in the output table is not a proper list."
                           value)))))
  (let ((expected (bags-to transition))
        (actual (hash-table-keys output)))
    (unless (alexandria:set-equal expected actual)
      (petri-net-error "Mismatched keys in output table.~%Expected:~S~%Found:~S"
                       expected actual))))

(defun transition-valid-p (transition petri-net &optional errorp)
  (let ((bags-from (bags-from transition)))
    (flet ((fn (name requested)
             (let ((actual (bag-count (bag petri-net name))))
               (cond ((<= requested actual))
                     (errorp (transition-not-ready name requested actual))
                     (t (return-from transition-valid-p nil))))))
      (maphash #'fn bags-from)
      t)))

;;; TRANSITION PROTOCOL

(defgeneric transition-call (transition))

(defgeneric populate-input (transition petri-net &optional skip-validation-p))

(defgeneric call-callback (transition input output))

(defgeneric populate-output (transition petri-net output))

;;; TRANSITION - IMPLEMENTATION

(defmethod transition-call ((transition transition))
  (named-lambda execute-transition (petri-net &optional skip-validation-p)
    (let* ((input (populate-input transition petri-net skip-validation-p))
           (output (make-hash-table)))
      (call-callback transition input output)
      (populate-output transition petri-net output))))

(defmethod populate-input
    ((transition transition) (petri-net petri-net) &optional skip-validation-p)
  (unless skip-validation-p
    (transition-valid-p transition petri-net t))
  (let ((input (make-hash-table)))
    (flet ((populate-input (name count)
             (setf (gethash name input)
                   (uiop:while-collecting (collect)
                     (dotimes (i count)
                       (collect (bag-remove (bag petri-net name))))))))
      (maphash #'populate-input (bags-from transition)))
    input))

(defmethod call-callback
    ((transition transition) (input hash-table) (output hash-table))
  (funcall (callback transition) input output))

(defmethod populate-output
    ((transition transition) (petri-net petri-net) (output hash-table))
  (validate-output-hash-table output transition)
  (dolist (name (bags-to transition))
    (dolist (token (gethash name output))
      (bag-insert (bag petri-net name) token))))

(defun make-transition (from to callback &optional (class 'transition))
  (setf from (ensure-list from)
        to (ensure-list to))
  (flet ((listify (forms)
           (uiop:while-collecting (collect)
             (dolist (thing forms)
               (if (consp thing)
                   (collect (cons (first thing) (second thing)))
                   (collect (cons thing 1)))))))
    (make-instance class :bags-from (alist-hash-table (listify from))
                         :bags-to to
                         :callback callback)))

;;; CONDITIONS

(define-condition petri-net-error (error) ())

(define-condition simple-petri-net-error (petri-net-error simple-condition) ())

(defun petri-net-error (control &rest args)
  (error 'simple-petri-net-error :format-control control
                                 :format-arguments args))

(define-condition transition-not-ready (petri-net-error)
  ((%requested-count :reader requested-count
                     :initarg :requested-count)
   (%actual-count :reader actual-count
                  :initarg :actual-count)
   (%bag :reader condition-bag
         :initarg :bag))
  (:default-initargs
   :requested-count (required-argument :requested-count)
   :actual-count (required-argument :actual-count)
   :bag (required-argument :bag))
  (:report transition-not-ready-report))

(defun transition-not-ready-report (condition stream)
  (format stream "Transition not ready: needed ~D tokens from bag ~S but ~
only ~D were available."
          (requested-count condition)
          (condition-bag condition)
          (actual-count condition)))

(defun transition-not-ready (bag requested-count actual-count)
  (error 'transition-not-ready :requested-count requested-count
                               :bag bag
                               :actual-count actual-count))

;;; MACRO DEFINITION

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun form-edges (form)
    (flet ((test (x y) (and (symbolp x) (symbolp y) (string-equal x y))))
      (let ((sublists (split-sequence '-> form :test #'test)))
        (loop for (sublist-1 sublist-2) on sublists
              while (and sublist-1 sublist-2)
              nconc (map-product #'list sublist-1 sublist-2)))))

  (defun bag-form-p (thing)
    (or (symbolp thing)
        (and (proper-list-p thing)
             (= 2 (length thing))
             (symbolp (first thing))
             (positive-integer-p (second thing)))))

  (defun function-form-p (thing)
    (and (proper-list-p thing)
         (= 2 (length thing))
         (eql 'function (first thing))
         (symbolp (second thing))))

  (flet ((%edges-objects (edges predicate)
           (let ((hash-table (make-hash-table :test #'equal)))
             (dolist (edge edges)
               (cond ((funcall predicate (first edge))
                      (setf (gethash (first edge) hash-table) t))
                     ((funcall predicate (second edge))
                      (setf (gethash (second edge) hash-table) t))))
             (hash-table-keys hash-table))))
    (defun edges-transitions (edges)
      (%edges-objects edges #'function-form-p))
    (defun edges-bags (edges)
      (%edges-objects edges #'bag-form-p)))

  (flet ((%edges-bags (edges function-form key-fn value-fn)
           (flet ((fn (x) (equal (funcall value-fn x) function-form)))
             (mapcar key-fn (remove-if-not #'fn edges)))))
    (defun edges-bags-from (edges function-form)
      (%edges-bags edges function-form #'first #'second))
    (defun edges-bags-to (edges function-form)
      (%edges-bags edges function-form #'second #'first))))

(defmacro petri-net ((&optional (constructor '#'make-petri-net)) &body forms)
  (let* ((edges (mapcan #'form-edges forms))
         (transitions (edges-transitions edges))
         (bags (edges-bags edges)))
    (flet ((make-transition-form (x)
             `(list ',(edges-bags-from edges x)
                    ',(edges-bags-to edges x)
                    ,x)))
      `(funcall ,constructor
                ',bags
                (list ,@(mapcar #'make-transition-form transitions))))))

;;; THREADED

(defclass threaded-petri-net (petri-net)
  ((%lock :reader lock
          :initform (bt:make-lock))
   (%thread-queue :reader thread-queue
                  :initform (lparallel.queue:make-queue)))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod petri-net-transition-constructor ((petri-net threaded-petri-net))
  #'make-threaded-transition)

(defmethod petri-net-call ((petri-net threaded-petri-net))
  (named-lambda execute-threaded-petri-net ()
    (bt:with-lock-held ((lock petri-net))
      (spawn-transitions petri-net))
    (join-all-threads petri-net)))

(defun spawn-transitions (petri-net)
  (flet ((spawn ()
           (when-let ((transition (find-valid-transition petri-net)))
             (let ((input (populate-input transition petri-net t)))
               (bt:make-thread (curry transition input petri-net))))))
    (loop with queue = (thread-queue petri-net)
          for thread = (spawn)
          while thread do (lparallel.queue:push-queue thread queue))))

(defun join-all-threads (petri-net)
  (loop with queue = (thread-queue petri-net)
        for thread = (lparallel.queue:try-pop-queue queue)
        while thread
        do (multiple-value-bind (condition backtrace) (bt:join-thread thread)
             (when (typep condition 'condition)
               (threaded-petri-net-error condition backtrace)))))

(defclass threaded-transition (transition) ()
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod transition-call ((transition threaded-transition))
  (named-lambda execute-threaded-transition (input petri-net)
    (handler-bind ((error (lambda (e)
                            (return-from execute-threaded-transition
                              (values e (print-backtrace e :output nil))))))
      (let ((output (make-hash-table)))
        (call-callback transition input output)
        (bt:with-lock-held ((lock petri-net))
          (populate-output transition petri-net output)
          (spawn-transitions petri-net))
        (values nil nil)))))

(defun make-threaded-transition (from to callback)
  (make-transition from to callback 'threaded-transition))

(defun make-threaded-petri-net (bags transitions)
  (make-instance 'threaded-petri-net :bags bags :transitions transitions))

(defmacro threaded-petri-net
    ((&optional (constructor '#'make-threaded-petri-net)) &body forms)
  `(petri-net (,constructor) ,@forms))

(define-condition threaded-petri-net-error (petri-net-error)
  ((%reason :reader reason
            :initarg :reason
            :initform (required-argument :reason))
   (%backtrace :reader backtrace
               :initarg :backtrace
               :initform nil))
  (:report (lambda (condition stream)
             (format stream "Error while executing the threaded Petri net:~%~A"
                     (reason condition)))))

(defun threaded-petri-net-error (reason backtrace)
  (error 'threaded-petri-net-error :reason reason :backtrace backtrace))

;; TODO split threaded from nonthreaded
;; TODO graphs
;; TODO validation in macro declaration
;; TODO inhibitors
;; TODO declare (foo -> #'bar -> (baz 3)) to mark that exactly three tokens
;; are supposed to be output from #'bar - validate it

;; (petri-net ()
;;   (credentials -> #'login -> cookie-jars
;;                -> #'dl-account -> accounts)
;;   (accounts -> #'dl-images -> images)
;;   (accounts -> #'dl-furres
;;             -> furres-for-costumes furres-for-portraits furres-for-specitags)
;;   (furres-for-costumes -> #'dl-costumes -> costumes)
;;   (furres-for-portraits -> #'dl-portraits -> portraits)
;;   (furres-for-specitags -> #'dl-specitags -> specitags))

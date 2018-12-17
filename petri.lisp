;;;; petri.lisp

(uiop:define-package #:petri
  (:mix #:closer-mop
        #:cl
        #:alexandria
        #:phoe-toolbox/bag)
  (:reexport #:phoe-toolbox/bag)
  (:export #:transition #:bags-from #:bags-to #:callback
           #:make-transition
           #:transition-valid-p
           #:transition-not-ready
           #:transition-not-ready-requested-count
           #:transition-not-ready-actual-count
           #:transition-not-ready-bag
           #:petri-net #:bags #:transitions
           #:make-petri-net
           #:find-valid-transitions))

(in-package #:petri)

;;; BAG

(defun make-bags (&rest symbols)
  (assert (every #'symbolp symbols))
  (alist-hash-table (mapcar (lambda (x) (cons x (make-bag))) symbols)))

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

(defmethod print-object ((transition transition) stream)
  (print-unreadable-object (transition stream :type t)
    (format stream "(~S -> ~S)"
            (hash-table-keys (bags-from transition))
            (bags-to transition))))

(defmethod initialize-instance :after ((transition transition) &key)
  (set-funcallable-instance-function transition (transition-call transition)))

(defun transition-call (transition)
  (named-lambda execute-transition (petri-net &optional skip-validation-p)
    (unless skip-validation-p
      (transition-valid-p transition petri-net t))
    (let* ((input (collect-input petri-net (bags-from transition)))
           (output (make-hash-table)))
      (funcall (callback transition) input output)
      (transmit-output petri-net output (bags-to transition)))))

(defun transition-valid-p (transition petri-net &optional errorp)
  (let ((bags-from (bags-from transition)))
    (flet ((fn (name requested)
             (let ((actual (bag-count (bag petri-net name))))
               (cond ((<= requested actual))
                     (errorp (transition-not-ready name requested actual))
                     (t (return-from transition-valid-p nil))))))
      (maphash #'fn bags-from)
      t)))

(defun collect-input (petri-net bags-from)
  (let ((input (make-hash-table)))
    (flet ((fn (name count)
             (setf (gethash name input)
                   (uiop:while-collecting (collect)
                     (dotimes (i count)
                       (collect (bag-remove (bag petri-net name))))))))
      (maphash #'fn bags-from)
      input)))

(defun transmit-output (petri-net output bags-to)
  (dolist (name bags-to)
    (dolist (token (gethash name output))
      (bag-insert (bag petri-net name) token))))

(defun make-transition (from to callback)
  (setf from (ensure-list from))
  (setf to (ensure-list to))
  (flet ((listify (forms)
           (uiop:while-collecting (collect)
             (dolist (thing forms)
               (if (consp thing) (collect thing) (collect (cons thing 1)))))))
    (make-instance 'transition :bags-from (alist-hash-table (listify from))
                               :bags-to to
                               :callback callback)))

;;; TRANSITION-NOT-READY

(define-condition transition-not-ready (error)
  ((%requested-count :reader transition-not-ready-requested-count
                     :initarg :requested-count)
   (%actual-count :reader transition-not-ready-actual-count
                  :initarg :actual-count)
   (%bag :reader transition-not-ready-bag
         :initarg :bag))
  (:default-initargs
   :requested-count (required-argument :requested-count)
   :actual-count (required-argument :actual-count)
   :bag (required-argument :bag))
  (:report transition-not-ready-report))

(defun transition-not-ready-report (condition stream)
  (format stream "Transition not ready: needed ~D tokens from bag ~S but ~
only ~D were available."
          (transition-not-ready-requested-count condition)
          (transition-not-ready-bag condition)
          (transition-not-ready-actual-count condition)))

(defun transition-not-ready (bag requested-count actual-count)
  (error 'transition-not-ready :requested-count requested-count
                               :bag bag
                               :actual-count actual-count))

;;; PETRI-NET

(defclass petri-net ()
  ((%bags :accessor bags)
   (%transitions :accessor transitions
                 :initarg :transitions))
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs :bags (make-hash-table) :transitions '()))

(defmethod initialize-instance :after ((petri-net petri-net) &key bags)
  (set-funcallable-instance-function petri-net (petri-net-call petri-net))
  (setf (slot-value petri-net '%bags) (apply #'make-bags bags)))

(defun petri-net-call (petri-net)
  (named-lambda execute-petri-net ()
    (loop for transition = (find-valid-transition petri-net)
          while transition do (funcall transition petri-net t))
    petri-net))

(defmethod print-object ((petri-net petri-net) stream)
  (print-unreadable-object (petri-net stream :type t)
    (format stream "(~DP/~DT, ~A)"
            (hash-table-count (bags petri-net))
            (length (transitions petri-net))
            (handler-case
                (if (find-valid-transition petri-net) :ready :finished)
              (error () :error)))))

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

;;; TESTS

(defun test ()
  (flet ((callback (input output)
           (setf (gethash 'bar output)
                 (mapcar #'- (gethash 'foo input)))))
    (let* ((transitions (list (make-transition 'foo 'bar #'callback)))
           (petri-net (make-petri-net '(foo bar) transitions)))
      (mapcar (curry #'bag-insert (bag petri-net 'foo))
              '(0 1 2 3 4 5 6 7 8 9))
      (funcall petri-net)
      (values petri-net (bag-contents (bag petri-net 'bar))))))

(defun test2 ()
  (flet ((callback-1 (input output)
           (setf (gethash 'bar output) (mapcar #'- (gethash 'foo input))
                 (gethash 'baz output) (gethash 'foo input)))
         (callback-2 (input output)
           (setf (gethash 'quux output)
                 (append (gethash 'baz input) (gethash 'bar input)))))
    (let* ((transitions (list (make-transition 'foo '(bar baz) #'callback-1)
                              (make-transition '(bar baz) 'quux #'callback-2)))
           (petri-net (make-petri-net '(foo bar baz quux) transitions)))
      (mapcar (curry #'bag-insert (bag petri-net 'foo))
              '(1 2 3))
      (funcall petri-net)
      (values petri-net (bag-contents (bag petri-net 'quux))))))


















;;; MACRO DEFINITION

(defmacro make-petri-net (&body forms)
  (declare (ignore forms)))

;; TODO async
;; TODO tests
;; TODO macro declaration like below

(make-petri-net
  (credentials -> #'login -> cookie-jars
               -> #'dl-account -> accounts)
  (accounts -> #'dl-images -> images)
  (accounts -> #'dl-furres
            -> furres-for-costumes furres-for-portraits furres-for-specitags)
  (furres-for-costumes -> #'dl-costumes -> costumes)
  (furres-for-portraits -> #'dl-portraits -> portraits)
  (furres-for-specitags -> #'dl-specitags -> specitags))

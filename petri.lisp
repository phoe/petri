;;;; petri.lisp

(uiop:define-package #:petri
  (:mix #:closer-mop
        #:cl
        #:alexandria
        #:split-sequence
        #:phoe-toolbox/bag)
  (:reexport #:phoe-toolbox/bag)
  (:export #:transition #:bags-from #:bags-to #:callback
           #:make-transition
           #:transition-valid-p
           #:petri-net-error #:simple-petri-net-error
           #:transition-not-ready
           #:requested-count
           #:actual-count
           #:condition-bag
           #:petri-net #:bags #:transitions
           #:make-petri-net
           #:find-valid-transitions))

(in-package #:petri)

;;; BAG

(defun make-bags (&rest symbols)
  (assert (every #'symbolp symbols))
  (alist-hash-table (mapcar (lambda (x) (cons x (make-bag))) symbols)))

;;; PETRI-NET

(defclass petri-net ()
  ((%bags :accessor bags)
   (%transitions :accessor transitions
                 :initarg :transitions))
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs :bags (make-hash-table) :transitions '()))

(defmethod print-object ((petri-net petri-net) stream)
  (print-unreadable-object (petri-net stream :type t)
    (format stream "(~DP/~DT, ~A)"
            (hash-table-count (bags petri-net))
            (length (transitions petri-net))
            (handler-case
                (if (find-valid-transition petri-net) :ready :finished)
              (error () :error)))))

(defmethod initialize-instance :after ((petri-net petri-net) &key bags)
  (set-funcallable-instance-function petri-net (petri-net-call petri-net))
  (setf (slot-value petri-net '%bags)
        (apply #'make-bags (mapcar #'ensure-car bags))))

(defun petri-net-call (petri-net)
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

(defmethod print-object ((transition transition) stream)
  (print-unreadable-object (transition stream :type t)
    (format stream "(~S -> ~S)"
            (hash-table-keys (bags-from transition))
            (bags-to transition))))

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

(defun make-transition (from to callback)
  (setf from (ensure-list from)
        to (ensure-list to))
  (flet ((listify (forms)
           (uiop:while-collecting (collect)
             (dolist (thing forms)
               (if (consp thing)
                   (collect (cons (first thing) (second thing)))
                   (collect (cons thing 1)))))))
    (make-instance 'transition :bags-from (alist-hash-table (listify from))
                               :bags-to to
                               :callback callback)))

;;; PETRI-NET-ERROR

(define-condition petri-net-error (error) ())

(define-condition simple-petri-net-error (petri-net-error simple-condition) ())

(defun petri-net-error (control &rest args)
  (error 'simple-petri-net-error :format-control control
                                 :format-arguments args))

;;; TRANSITION-NOT-READY

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

(defmacro petri-net (() &body forms)
  (let* ((edges (mapcan #'form-edges forms))
         (transitions (edges-transitions edges))
         (bags (edges-bags edges)))
    (flet ((make-transition-form (x)
             `(make-transition ',(edges-bags-from edges x)
                               ',(edges-bags-to edges x)
                               ,x)))
      `(make-petri-net ',bags
         (list ,@(mapcar #'make-transition-form transitions))))))

;; ;; todo merge into split-sequence
;; (defun split-on-delimiter (original delimiter)
;;   (loop for list = original then tail
;;         for (head tail)
;;           = (loop for sublist on list
;;                   until (eql (car sublist) delimiter)
;;                   collect (car sublist) into first-part
;;                   finally (return (list first-part (cdr sublist))))
;;         until (endp list)
;;         collect head))

;; TODO async
;; TODO graphs
;; TODO validation in macro declaration
;; TODO bag description for phoe-toolbox
;; TODO declare (foo -> #'bar -> (baz 3)) to mark that exactly three tokens
;; are supposed to be output from #'bar

;; (petri-net ()
;;   (credentials -> #'login -> cookie-jars
;;                -> #'dl-account -> accounts)
;;   (accounts -> #'dl-images -> images)
;;   (accounts -> #'dl-furres
;;             -> furres-for-costumes furres-for-portraits furres-for-specitags)
;;   (furres-for-costumes -> #'dl-costumes -> costumes)
;;   (furres-for-portraits -> #'dl-portraits -> portraits)
;;   (furres-for-specitags -> #'dl-specitags -> specitags))

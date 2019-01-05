;;;; petri.lisp

(uiop:define-package #:petri
    (:mix #:closer-mop
          #:cl
          #:alexandria
          #:split-sequence
          #:phoe-toolbox/bag)
  (:reexport #:phoe-toolbox/bag)
  (:export #:petri-net
           #:make-petri-net
           #:bag-of
           #:bag-names
           #:petri-net-error
           #:simple-petri-net-error))

(in-package #:petri)

;;; BAG

(defun make-bags (&rest symbols)
  (assert (every #'symbolp symbols))
  (alist-hash-table (mapcar (lambda (x) (cons x (make-bag))) symbols)))

;;; PETRI NET PROTOCOL

(defgeneric petri-net-transition-constructor (petri-net))

(defgeneric make-petri-net-funcallable-function (petri-net))

;;; TRANSITION PROTOCOL

(defgeneric make-transition-funcallable-function (transition))

(defgeneric populate-input (transition petri-net &optional skip-validation-p))

(defgeneric call-callback (transition input output))

(defgeneric populate-output (transition petri-net output))

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
  (set-funcallable-instance-function
   petri-net (make-petri-net-funcallable-function petri-net))
  (let ((constructor (petri-net-transition-constructor petri-net)))
    (setf (transitions petri-net)
          (mapcar (curry #'apply constructor) transitions)))
  (setf (slot-value petri-net '%bags)
        (apply #'make-bags (mapcar #'ensure-car bags))))

(defmethod petri-net-transition-constructor ((petri-net petri-net))
  #'make-transition)

(defmethod make-petri-net-funcallable-function ((petri-net petri-net))
  (named-lambda execute-petri-net (&optional (compress t))
    (loop for transition = (find-ready-transition petri-net)
          while transition do (funcall transition petri-net t))
    (when compress
      (dolist (bag (hash-table-values (bags petri-net)))
        (bag-compress bag)))
    petri-net))

(defun bag-of (petri-net name)
  (gethash name (bags petri-net)))

(defun (setf bag-of) (new-value petri-net name)
  (let ((foundp (nth-value 1 (gethash name (bags petri-net)))))
    (if foundp
        (setf (gethash name (bags petri-net)) new-value)
        (remhash name (bags petri-net)))
    new-value))

(defun bag-names (petri-net)
  (hash-table-keys (bags petri-net)))

(defun find-ready-transition (petri-net)
  (find-if (rcurry #'transition-ready-p petri-net)
           (shuffle (copy-list (transitions petri-net)))))

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
                     :bags-to (make-hash-table)
                     :callback (required-argument :callback))
  (:metaclass closer-mop:funcallable-standard-class))

(defun no-one-element-lists (x)
  (if (and (consp x) (cdr x)) x (car x)))

(defmethod print-object ((transition transition) stream)
  (print-unreadable-object (transition stream :type t)
    (format stream "(~S -> ~S)"
            (no-one-element-lists (hash-table-keys (bags-from transition)))
            (no-one-element-lists (hash-table-keys (bags-to transition))))))

(defmethod initialize-instance :after ((transition transition) &key)
  (set-funcallable-instance-function
   transition (make-transition-funcallable-function transition)))

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
  (let* ((bags-to (bags-to transition))
         (expected (hash-table-keys bags-to))
         (actual (hash-table-keys output)))
    (unless (alexandria:set-equal expected actual)
      (petri-net-error "Mismatched keys in output table.~%Expected:~S~%Found:~S"
                       expected actual))
    (dolist (key expected)
      (let ((expected-count (gethash key bags-to)))
        (unless (eq expected-count '*)
          (let ((actual-count (length (gethash key output))))
            (unless (= expected-count actual-count)
              (petri-net-error "Mismatched element count for key ~S.
Expected ~D elements but found ~D." key expected-count actual-count))))))))

(defun transition-ready-p (transition petri-net &optional errorp)
  (let ((bags-from (bags-from transition)))
    (flet ((fn (name requested)
             (let ((actual (bag-count (bag-of petri-net name))))
               (cond ((and (symbolp requested)
                           (string= '! requested)
                           (/= 0 actual))
                      (return-from transition-ready-p nil))
                     ((and (symbolp requested)
                           (string= '! requested)
                           (= 0 actual)))
                     ((<= requested actual))
                     (errorp (transition-not-ready name requested actual))
                     (t (return-from transition-ready-p nil))))))
      (maphash #'fn bags-from)
      t)))

;;; TRANSITION - IMPLEMENTATION

(defmethod make-transition-funcallable-function ((transition transition))
  (named-lambda execute-transition (petri-net &optional skip-validation-p)
    (let* ((input (populate-input transition petri-net skip-validation-p))
           (output (make-output-hash-table transition)))
      (call-callback transition input output)
      (populate-output transition petri-net output))))

(defun make-output-hash-table (transition)
  (let ((hash-table (make-hash-table)))
    (dolist (symbol (hash-table-keys (bags-to transition)))
      (setf (gethash symbol hash-table) nil))
    hash-table))

(defmethod populate-input
    ((transition transition) (petri-net petri-net) &optional skip-validation-p)
  (unless skip-validation-p
    (transition-ready-p transition petri-net t))
  (let ((input (make-hash-table)))
    (flet ((populate-input (name count)
             (unless (and (symbolp count)
                          (string= '! count))
               (setf (gethash name input)
                     (uiop:while-collecting (collect)
                       (dotimes (i count)
                         (collect (bag-remove (bag-of petri-net name)))))))))
      (maphash #'populate-input (bags-from transition)))
    input))

(defmethod call-callback
    ((transition transition) (input hash-table) (output hash-table))
  (funcall (callback transition) input output))

(defmethod populate-output
    ((transition transition) (petri-net petri-net) (output hash-table))
  (validate-output-hash-table output transition)
  (dolist (name (hash-table-keys (bags-to transition)))
    (dolist (token (gethash name output))
      (bag-insert (bag-of petri-net name) token))))

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
                         :bags-to (alist-hash-table (listify to))
                         :callback callback)))

;;; CONDITIONS

(define-condition petri-net-error (error) ())

(define-condition simple-petri-net-error (petri-net-error simple-condition) ())

(defun petri-net-error (control &rest args)
  (error 'simple-petri-net-error :format-control control
                                 :format-arguments args))

;;; TODO remove it, we never actually use it

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
    (unless (proper-list-p form)
      (petri-net-error "Malformed form: ~S" form))
    (flet ((arrowp (x) (and (symbolp x) (string-equal x "->"))))
      (let ((sublists (split-sequence-if #'arrowp form)))
        (cond ((= 0 (length sublists))
               (petri-net-error "Empty edge in macro form."))
              ((= 1 (length sublists))
               (petri-net-error "Malformed edge: ~S" (first sublists))))
        (loop for (sublist-1 sublist-2) on sublists
              while (and sublist-1 sublist-2)
              nconc (map-product #'list sublist-1 sublist-2)))))

  (defun bag-form-p (thing)
    (or (symbolp thing)
        (and (proper-list-p thing)
             (= 2 (length thing))
             (symbolp (first thing))
             (or (and (symbolp (second thing))
                      (string= '* (second thing)))
                 (and (symbolp (second thing))
                      (string= '! (second thing)))
                 (positive-integer-p (second thing))))))

  (defun inhibitor-bag-form-p (thing)
    (and (proper-list-p thing)
         (= 2 (length thing))
         (symbolp (first thing))
         (symbolp (second thing))
         (string= '! (second thing))))

  (defun wildcard-bag-form-p (thing)
    (and (proper-list-p thing)
         (= 2 (length thing))
         (symbolp (first thing))
         (symbolp (second thing))
         (string= '* (second thing))))

  (defun function-form-p (thing)
    (and (proper-list-p thing)
         (= 2 (length thing))
         ;; We do not use (EQL 'FUNCTION (FIRST THING)) to take modified
         ;; readtables into account - for example, readtable :QTOOLS uses
         ;; its own custom version of the #' reader macro that evaluates to
         ;; (CL+QT:FUNCTION thing).
         (symbolp (first thing))
         (string= 'function (first thing))
         (symbolp (second thing))))

  (defun edges-objects (edges)
    (let ((transitions (make-hash-table))
          (bags (make-hash-table)))
      (dolist (edge edges)
        (destructuring-bind (first second) edge
          (cond ((wildcard-bag-form-p first)
                 (petri-net-error "Wildcard bag ~S is not allowed as input."
                                  first))
                ((inhibitor-bag-form-p second)
                 (petri-net-error "Inhibitor bag ~S is not allowed as output."
                                  second))
                ((and (function-form-p first)
                      (bag-form-p second))
                 (setf (gethash first transitions) t
                       (gethash (ensure-car second) bags) t))
                ((and (bag-form-p first)
                      (function-form-p second))
                 (setf (gethash (ensure-car first) bags) t
                       (gethash second transitions) t))
                (t
                 (petri-net-error "Malformed Petri net edge from ~S to ~S."
                                  first second)))))
      (values (hash-table-keys transitions)
              (hash-table-keys bags))))

  (flet ((%edges-bags (edges function-form key-fn value-fn)
           (flet ((fn (x) (equal (funcall value-fn x) function-form)))
             (remove-duplicates (mapcar key-fn (remove-if-not #'fn edges))
                                :test #'equal))))
    (defun edges-bags-from (edges function-form)
      (%edges-bags edges function-form #'first #'second))
    (defun edges-bags-to (edges function-form)
      (%edges-bags edges function-form #'second #'first))))

(defmacro %petri-net (constructor &body forms)
  (let ((edges (mapcan #'form-edges forms)))
    (multiple-value-bind (transitions bags) (edges-objects edges)
      (flet ((make-transition-form (x)
               `(list ',(edges-bags-from edges x)
                      ',(edges-bags-to edges x)
                      ,x)))
        `(funcall ,constructor
                  ',bags
                  (list ,@(remove-duplicates
                           (mapcar #'make-transition-form transitions)
                           :test #'equal)))))))

(defmacro petri-net (() &body forms)
  `(%petri-net #'make-petri-net ,@forms))

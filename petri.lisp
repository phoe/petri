;;;; petri.lisp

(uiop:define-package #:petri
  (:mix #:closer-mop
        #:cl
        #:alexandria))

(in-package #:petri)

;;; PLACE

;;; TODO move to PHOE-TOOLBOX/BAG system
(defstruct (place (:constructor make-place ())
                  (:print-function print-place))
  (contents (make-array 0 :fill-pointer t :adjustable t) :type (array t (*))))

(defun print-place (object stream depth)
  (declare (ignore depth))
  (print-unreadable-object (object stream :type t)
    (format stream "(~A)" (fill-pointer (place-contents object)))))

(defun place-insert (place element)
  (check-type place place)
  (vector-push-extend element (place-contents place))
  (values))

(defun place-count (place)
  (check-type place place)
  (fill-pointer (place-contents place)))

(defun place-remove (place)
  (check-type place place)
  (unless (positive-integer-p (place-count place))
    (error "Cannot remove items from an empty place."))
  (let* ((count (place-count place))
         (n (random count))
         (result (aref (place-contents place) n)))
    (setf (aref (place-contents place) n)
          (aref (place-contents place) (1- count)))
    (decf (fill-pointer (place-contents place)))
    result))

(defun place-compress (place)
  (adjust-array (place-contents place) (place-count place))
  (values))

(defun make-places (&rest symbols)
  (assert (every #'symbolp symbols))
  (alist-hash-table (mapcar (lambda (x) (cons x (make-place))) symbols)))

;;; TRANSITION

(defclass transition ()
  ((%places-from :accessor places-from
                 :initarg :places-from)
   (%places-to :accessor places-to
               :initarg :places-to)
   (%callback :accessor callback
              :initarg :callback))
  (:default-initargs :places-from (make-hash-table)
                     :places-to '()
                     :callback (required-argument :callback))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod print-object ((transition transition) stream)
  (print-unreadable-object (transition stream :type t)
    (format stream "(~S -> ~S)"
            (hash-table-keys (places-from transition))
            (places-to transition))))

(defmethod initialize-instance :after ((transition transition) &key)
  (set-funcallable-instance-function
   transition (transition-funcall transition)))

(defun transition-funcall (transition)
  (named-lambda execute-transition (petri-net &optional skip-validation-p)
    (unless skip-validation-p
      (transition-valid-p transition petri-net t))
    (let* ((input (collect-input petri-net (places-from transition)))
           (output (make-hash-table)))
      (funcall (callback transition) input output)
      (transmit-output petri-net output (places-to transition)))))

(defun transition-not-ready (name requested-count actual-count)
  (error "Transition not ready: needed ~D tokens from place ~S but only ~D ~
were available." requested-count name actual-count))

(defun transition-valid-p (transition petri-net &optional errorp)
  (let ((places-from (places-from transition)))
    (flet ((fn (name requested)
             (let ((actual (place-count (place petri-net name))))
               (cond ((<= requested actual))
                     (errorp (transition-not-ready name requested actual))
                     (t (return-from transition-valid-p nil))))))
      (maphash #'fn places-from)
      t)))

(defun collect-input (petri-net places-from)
  (let ((input (make-hash-table)))
    (flet ((fn (name count)
             (setf (gethash name input)
                   (uiop:while-collecting (collect)
                     (dotimes (i count)
                       (collect (place-remove (place petri-net name))))))))
      (maphash #'fn places-from)
      input)))

(defun transmit-output (petri-net output places-to)
  (dolist (name places-to)
    (dolist (token (gethash name output))
      (place-insert (place petri-net name) token))))

(defun make-transition (from to callback)
  (setf from (ensure-list from))
  (setf to (ensure-list to))
  (flet ((listify (forms)
           (uiop:while-collecting (collect)
             (dolist (thing forms)
               (if (consp thing) (collect thing) (collect (cons thing 1)))))))
    (make-instance 'transition :places-from (alist-hash-table (listify from))
                               :places-to to
                               :callback callback)))

;;; PETRI-NET

(defclass petri-net ()
  ((%places :accessor places)
   (%transitions :accessor transitions
                 :initarg :transitions))
  (:default-initargs :places (make-hash-table) :transitions '()))

(defmethod initialize-instance :after ((petri-net petri-net) &key places)
  (setf (slot-value petri-net '%places) (apply #'make-places places)))

(defmethod print-object ((petri-net petri-net) stream)
  (print-unreadable-object (petri-net stream :type t)
    (format stream "(~DP/~DT, ~A)"
            (hash-table-count (places petri-net))
            (length (transitions petri-net))
            (handler-case
                (if (find-valid-transition petri-net) :ready :finished)
              (error () :error)))))

(defgeneric place (petri-net name)
  (:method ((petri-net petri-net) (name symbol))
    (gethash name (places petri-net))))

(defgeneric (setf place) (new-value petri-net name)
  (:method ((new-value symbol) (petri-net petri-net) (name symbol))
    (multiple-value-bind (value foundp) (gethash name (places petri-net))
      (if new-value
          (setf (gethash name (places petri-net)) new-value)
          (remhash name (places petri-net)))
      new-value)))

(defun find-valid-transition (petri-net)
  (find-if (rcurry #'transition-valid-p petri-net)
           (shuffle (transitions petri-net))))

(defun execute (petri-net)
  (loop for transition = (find-valid-transition petri-net)
        while transition do (funcall transition petri-net t))
  petri-net)

(defun make-petri-net (places transitions)
  (make-instance 'petri-net :places places :transitions transitions))

(defun test ()
  (flet ((callback (input output)
           (setf (gethash 'bar output)
                 (mapcar #'- (gethash 'foo input)))))
    (let* ((transitions (list (make-transition 'foo 'bar #'callback)))
           (petri-net (make-petri-net '(foo bar) transitions)))
      (mapcar (curry #'place-insert (place petri-net 'foo))
              '(0 1 2 3 4 5 6 7 8 9))
      (execute petri-net)
      (values petri-net (place-contents (place petri-net 'bar))))))

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
      (mapcar (curry #'place-insert (place petri-net 'foo))
              '(1 2 3))
      (execute petri-net)
      (values petri-net (place-contents (place petri-net 'quux))))))


















;;; MACRO DEFINITION

(defmacro define-petri-net (&rest forms)
  (declare (ignore forms)))

;;; EXAMPLE

(define-petri-net raptor-picker ()
  (credentials -> #'login -> cookie-jars)
  (cookie-jars -> #'dl-account -> accounts)
  (accounts -> #'dl-images -> images)
  (accounts -> #'dl-furres
            -> furres-for-costumes furres-for-portraits furres-for-specitags)
  (furres-for-costumes -> #'dl-costumes -> costumes)
  (furres-for-portraits -> #'dl-portraits -> portraits)
  (furres-for-specitags -> #'dl-specitags -> specitags))

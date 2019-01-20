;;;; graph.lisp

(defpackage #:petri/graph
  (:use #:cl
        #:alexandria
        #:petri)
  (:export #:display-graph))

(in-package #:petri/graph)

(defmethod cl-dot:generate-graph ((petri-net petri-net) &optional attributes)
  (cl-dot:generate-graph-from-roots 'petri-net (petri::transitions petri-net)
                                    attributes))

(defun make-object-from-bag (key count)
  (let* ((attributes `(,@(when (and (symbolp count)
                                    (string= count '!))
                           '(:dir :both
                             :arrowtail :box))
                       ,@(when (and (symbolp count)
                                    (string= count '*))
                           '(:label " *"))
                       ,@(when (and (integerp count)
                                    (< 1 count))
                           `(:label ,(format nil " ~D" count)))
                       :fontname "times bold"
                       :labeldistance 10
                       :color :black)))
    (make-instance 'cl-dot:attributed
                   :object key
                   :attributes attributes)))

(defmethod cl-dot:graph-object-pointed-to-by
    ((graph (eql 'petri-net)) (transition petri::transition))
  (uiop:while-collecting (collect)
    (let ((bags-from (petri::bags-from transition)))
      (dolist (key (hash-table-keys bags-from))
        (let ((count (gethash key bags-from)))
          (collect (make-object-from-bag key count)))))))

(defmethod cl-dot:graph-object-points-to
    ((graph (eql 'petri-net)) (transition petri::transition))
  (uiop:while-collecting (collect)
    (let ((bags-to (petri::bags-to transition)))
      (dolist (key (hash-table-keys bags-to))
        (let ((count (gethash key bags-to)))
          (collect (make-object-from-bag key count)))))))

(defmethod cl-dot:graph-object-node
    ((graph (eql 'petri-net)) (transition petri::transition))
  (let* ((callback (petri::callback transition))
         (name (princ-to-string
                (or (nth-value 2 (function-lambda-expression callback))
                    callback))))
    (make-instance 'cl-dot:node
                   :attributes `(:label ,name
                                 :shape :box
                                 :style :filled
                                 :color :black
                                 :fontcolor :white
                                 :fillcolor "#444444"
                                 :fixedsize t))))

(defmethod cl-dot:graph-object-node
    ((graph (eql 'petri-net)) (bag symbol))
  (make-instance 'cl-dot:node
                 :attributes `(:label ,(symbol-name bag)
                               :shape :ellipse
                               :fixedsize t)))

(defun generate-graph-to-file (petri-net &optional (rankdir "TB"))
  (let ((graph (cl-dot:generate-graph petri-net `(:rankdir ,rankdir))))
    (uiop:with-temporary-file (:pathname pathname :keep t
                               :prefix "petri-net-" :type "png")
      (let ((namestring (uiop:native-namestring pathname)))
        (cl-dot:dot-graph graph namestring :format :png)
        pathname))))

(defun display-graph (petri-net)
  (let ((pathname (generate-graph-to-file petri-net)))
    #+windows (error "Not implemented")
    #-windows (uiop:run-program (list "xdg-open" (namestring pathname)))))

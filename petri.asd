;;;; petri.asd

(asdf:defsystem #:petri
  :description "An implementation of Petri nets"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:closer-mop
               #:phoe-toolbox/bag)
  :components ((:file "petri")))

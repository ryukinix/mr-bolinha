;;;; mr-bolinha.asd

(asdf:defsystem #:mr-bolinha
  :description "mr-bolinha"
  :author "Manoel Vilela & Luigi Vasconcelos & Diogo Vilela"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (trivial-gamekit)
  :components ((:file "package")
               (:file "mr-bolinha")))

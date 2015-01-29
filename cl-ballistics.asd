;;;; cl-ballistics.asd

(asdf:defsystem #:cl-ballistics
  :description "A library to deal with ballistics data and produce ballistics solutions."
  :author "Samuel W. Flint <swflint@flintfam.org"
  :license "GNU GPL"
  :depends-on (#:cl-mathstats
               #:iterate)
  :serial t
  :components ((:file "package")
               (:file "cl-ballistics")))


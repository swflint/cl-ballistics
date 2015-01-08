;;;; cl-ballistics.asd

(asdf:defsystem #:cl-ballistics
  :description "Describe cl-ballistics here"
  :author "Samuel W. Flint <swflint@flintfam.org"
  :license "GNU GPL"
  :depends-on (#:cl-mathstats)
  :serial t
  :components ((:file "package")
               (:file "cl-ballistics")))


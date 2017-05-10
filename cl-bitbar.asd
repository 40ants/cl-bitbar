;;;; cl-bitbar.asd

(asdf:defsystem #:cl-bitbar
  :description "Utils to build BitBar plugins in CommonLisp."
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "BSD"
  :depends-on (#:cl-strings
               #:cl-interpol
               #:rutils
               #:iterate)
  :components ((:module "src"
                :serial t
                :components ((:file "utils")
                             (:file "bitbar")))))


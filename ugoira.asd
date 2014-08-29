;;;; ugoira.asd

(asdf:defsystem #:ugoira
  :serial t
  :description "Download pixiv animations"
  :author "Timofei Shatrov <timofei.shatrov@gmail.com>"
  :license "MIT"
  :depends-on (#:webgunk/modules 
               #:drakma 
               #:cl-ppcre
               #:cl-interpol)
  :components ((:file "package")
               (:file "ugoira")))







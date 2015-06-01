
(defsystem :temporary-file
  :description "Temporary file creation library"
  :author "Hans Hübner <hans.huebner@gmail.com>"
  :license "BSD"
  :serial t
  :depends-on (:alexandria
               :bordeaux-threads
               :cl-fad
               :cl-ppcre
               :unit-test)
  :components ((:file "temporary-file")))

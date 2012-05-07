
(defsystem :temporary-file
    :description "Temporary file creation library"
  :serial t
  :depends-on (:alexandria
               :bordeaux-threads
               :cl-fad
               :cl-ppcre
               :unit-test)
  :components ((:file "temporary-file")))

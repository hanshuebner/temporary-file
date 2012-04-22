
(defsystem :temporary-file
    :description "Temporary file creation library"
  :serial t
  :depends-on (:alexandria :bordeaux-threads :cl-fad)
  :components ((:file "temporary-file")))

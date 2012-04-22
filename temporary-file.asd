
(defsystem :temporary-file
    :description "Temporary file creation library"
  :serial t
  :depends-on (:alexandria)
  :components ((:file "temporary-file")))

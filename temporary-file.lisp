(defpackage :temporary-file
  (:use :cl)
  (:export #:open-temporary
           #:with-open-temporary-file))

(in-package :temporary-file)

(eval-when (:load-toplevel :execute)
  (when (null (logical-pathname-translations "TEMPORARY-FILES"))
    (alexandria:if-let (default-temporary-directory #-windows (load-time-value (or (directory-from-environment "TMPDIR")
                                                                                   (probe-file #P"/tmp/")))
                                                    #+windows (load-time-value (or (directory-from-environment "TEMP")
                                                                                   (error 'missing-temp-environment-variable))))
      (setf (logical-pathname-translations "TEMPORARY-FILES") `(("*.*.*" ,*default-temporary-directory*)))
      (warn "could not automatically determine a default mapping for TEMPORARY-FILES"))))

(define-condition missing-temp-environment-variable (error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "the TEMP environment variable has not been found, cannot continue"))))

(defparameter *max-tries* 10000)

(defvar *temporary-file-random-state* (make-random-state t))

;; from XCVB
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun getenv (x)
    "Query the libc runtime environment. See getenv(3)."
    (declare (ignorable x))
    #+(or abcl clisp xcl) (ext:getenv x)
    #+allegro (sys:getenv x)
    #+clozure (ccl:getenv x)
    #+(or cmu scl) (cdr (assoc x ext:*environment-list* :test #'string=))
    #+cormanlisp
    (let* ((buffer (ct:malloc 1))
           (cname (ct:lisp-string-to-c-string x))
           (needed-size (win:getenvironmentvariable cname buffer 0))
           (buffer1 (ct:malloc (1+ needed-size))))
      (prog1 (if (zerop (win:getenvironmentvariable cname buffer1 needed-size))
                 nil
                 (ct:c-string-to-lisp-string buffer1))
        (ct:free buffer)
        (ct:free buffer1)))
    #+ecl (si:getenv x)
    #+gcl (system:getenv x)
    #+lispworks (lispworks:environment-variable x)
    #+mcl (ccl:with-cstrs ((name x))
            (let ((value (_getenv name)))
              (unless (ccl:%null-ptr-p value)
                (ccl:%get-cstring value))))
    #+sbcl (sb-ext:posix-getenv x)
    #-(or abcl allegro clisp clozure cmu cormanlisp ecl gcl lispworks mcl sbcl scl xcl)
    (error "~S is not supported on your implementation" 'getenv))

  (defun directory-from-environment (environment-variable-name)
    (let ((string (getenv environment-variable-name)))
      (when string
        (cl-fad:pathname-as-directory string)))))

;; locking for multi-threaded operation with unsafe random function

(defvar *create-file-name-lock* (bordeaux-threads:make-lock "Temporary File Name Creation Lock"))

(defmacro with-file-name-lock-held (() &body body)
  `(bordeaux-threads:with-lock-held (*create-file-name-lock*)
     ,@body))

(defun generate-random-string ()
  (with-file-name-lock-held ()
    (format nil "~:@(~36,8,'0R~)" (random (expt 36 8) *temporary-file-random-state*))))

(define-condition invalid-temporary-pathname-template (error)
  ((string :initarg :string))
  (:report (lambda (condition stream)
             (with-slots (string) condition
               (format stream "invalid temporary file name template ~S, must contain a percent ~
                               sign that is to be replaced by a random string" string)))))

(defun generate-random-pathname (defaults-string random-string-generator)
  (let ((percent-position (or (position #\% defaults-string)
                              (error 'invalid-temporary-pathname-template :string defaults-string))))
    (merge-pathnames (concatenate 'string
                                  (subseq defaults-string 0 percent-position)
                                  (funcall random-string-generator)
                                  (subseq defaults-string (1+ percent-position))))))

(define-condition cannot-create-temporary-file (error)
  ((defaults :initarg :defaults)
   (max-tries :initarg :max-tries))
  (:report (lambda (condition stream)
             (with-slots (defaults max-tries) condition
               (format stream "cannot create temporary file with defaults ~A, giving up after ~D attempt~:P"
                       defaults max-tries)))))

(defun open-temporary (&rest open-arguments
		       &key
                         (defaults "TEMPORARY-FILES:TEMP-%")
			 (generate-random-string 'generate-random-string)
                         (max-tries *max-tries*)
			 &allow-other-keys)
  "Create a file with a randomly generated name and return the opened
   stream.  The resulting pathname is generated from DEFAULTS, which
   is a string representing a pathname template.  A percent sign (%)
   in that string is replaced by a randomly generated string to make
   the filename unique.  The default for DEFAULTS places temporary
   files in the TEMPORARY-FILES logical pathname host, which is
   automatically set up in a system specific manner.  The DEFAULTS are
   merged with *DEFAULT-PATHNAME-DEFAULTS*, so random pathnames
   relative to that directory can be generated by not specifying a
   directory in DEFAULTS.

   GENERATE-RANDOM-STRING can be passed to override the default
   function that generates the random name component.  It should
   return a random string consisting of characters that are permitted
   in a pathname (logical or physical, depending on DEFAULTS).

   The name of the temporary file can be accessed calling the PATHNAME
   function on STREAM.  For convenience, the temporary file is opened
   on the physical pathname, i.e. if the DEFAULTS designate a logical
   pathname the translation to a physical pathname is performed before
   opening the stream.

   In order to create a unique file name, OPEN-TEMPORARY may loop
   internally up to MAX-TRIES times before giving up and signalling a
   CANNOT-CREATE-TEMPORARY-FILE condition."
  (loop thereis (apply #'open
                       (translate-logical-pathname (generate-random-pathname defaults generate-random-string))
                       :direction :output
                       :if-exists nil
                       (alexandria:remove-from-plist open-arguments :defaults :generate-random-string :max-tries))
        repeat max-tries
        finally (error 'cannot-create-temporary-file
                       :defaults defaults
                       :max-tries max-tries)))

(defmacro with-open-temporary-file ((stream &rest args &key (keep t)) &body body)
  "Create a temporary file using OPEN-TEMPORARY with ARGS and run BODY
  with STREAM bound to the temporary file stream.  See OPEN-TEMPORARY
  for permitted options.  If KEEP is set to NIL, the file is deleted
  when the body is exited."
  `(with-open-stream (,stream (open-temporary ,@args))
     (unwind-protect
          (progn ,@body)
       (unless ,keep
         (when (probe-file (pathname ,stream))
           (delete-file (pathname ,stream)))))))

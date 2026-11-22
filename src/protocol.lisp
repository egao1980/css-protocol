;;;; Backends. Class, variable and GFs GIVEN; COMPILE-STYLESHEET's method on T
;;;; is a STUB.
;;;;
;;;; COMPILE-STYLESHEET source &key backend
;;;;   - a STYLESHEET source is returned as is (EQ);
;;;;   - otherwise the backend is BACKEND, else *CSS-BACKEND*; with one, return
;;;;     (backend-compile-stylesheet backend source);
;;;;   - with none, signal CSS-NO-BACKEND (source = SOURCE) inside
;;;;     RESTART-CASE: USE-VALUE (stylesheet) returns it; USE-BACKEND
;;;;     (backend) retries with that backend.
;;;; Backends may also add COMPILE-STYLESHEET methods on their own source
;;;; classes (dispatch on source kind).

(in-package #:css-protocol)

(defvar *css-backend* nil
  "Default backend for COMPILE-STYLESHEET. NIL: none bound.")

(defclass css-backend () ()
  (:documentation "Base class for css-protocol backends."))

(defgeneric backend-compile-stylesheet (backend source &key &allow-other-keys)
  (:documentation "Compile SOURCE into a STYLESHEET. Backends specialise BACKEND."))

(defgeneric compile-stylesheet (source &key backend &allow-other-keys)
  (:documentation "SOURCE -> STYLESHEET through a backend."))

(defmethod compile-stylesheet ((source stylesheet) &key backend &allow-other-keys)
  (declare (ignore backend))
  source)

(defun %read-object (prompt)
  (format *query-io* "~&~A (evaluated): " prompt)
  (force-output *query-io*)
  (list (eval (read *query-io*))))

(defmethod compile-stylesheet (source &rest keys &key backend &allow-other-keys)
  (let ((options (loop for (key value) on keys by #'cddr
                       unless (eq key :backend) nconc (list key value))))
    (block compile
      (loop
        (let ((chosen (or backend *css-backend*)))
          (when chosen
            (return-from compile
              (apply #'backend-compile-stylesheet chosen source options))))
        (restart-case (error 'css-no-backend :source source)
          (use-value (stylesheet)
            :report "Return a stylesheet instead of compiling."
            :interactive (lambda () (%read-object "Stylesheet"))
            (return-from compile (%require-type stylesheet 'stylesheet)))
          (use-backend (new-backend)
            :report "Retry the compile with another backend."
            :interactive (lambda () (%read-object "Backend"))
            (setf backend new-backend)))))))

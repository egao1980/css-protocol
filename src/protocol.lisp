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

(defmethod compile-stylesheet (source &key backend &allow-other-keys)
  (declare (ignore source backend))
  (%css0-pending 'compile-stylesheet))

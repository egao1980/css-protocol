;;;; Conditions and restarts. GIVEN (not a stub), except where marked.
;;;;
;;;; Every validating constructor (make-*, parse-selector-list, css-ident)
;;;; signals a CSS-SYNTAX-ERROR subclass with CSS-ERROR-INPUT = the offending
;;;; text, inside RESTART-CASE with two restarts:
;;;;
;;;;   USE-VALUE (value)  - CL's restart: use VALUE instead. The replacement is
;;;;                        validated again (it may signal again).
;;;;   SKIP-INVALID ()    - give up on this item: the constructor returns NIL.
;;;;                        Containers (make-stylesheet, make-rule, the at-rule
;;;;                        constructors, make-keyframes-rule, make-keyframe)
;;;;                        drop NIL children.
;;;;
;;;; parse-selector-list's USE-VALUE takes replacement selector text;
;;;; SKIP-INVALID makes it return NIL. In make-rule a skipped selector list
;;;; makes the rule itself NIL (a rule with no selectors is not a rule).
;;;;
;;;; COMPILE-STYLESHEET with no backend signals CSS-NO-BACKEND with:
;;;;   USE-VALUE (stylesheet) - return it as the result;
;;;;   USE-BACKEND (backend)  - retry the compile with BACKEND.

(in-package #:css-protocol)

(define-condition css-error (error)
  ((message :initarg :message :reader css-error-message :initform nil))
  (:report (lambda (c s)
             (format s "CSS error~@[: ~A~]" (css-error-message c))))
  (:documentation "Root of every css-protocol condition."))

(define-condition css-syntax-error (css-error)
  ((input :initarg :input :reader css-error-input :initform nil))
  (:report (lambda (c s)
             (format s "~A~@[: ~A~]: ~S"
                     (css-syntax-error-kind c)
                     (css-error-message c)
                     (css-error-input c))))
  (:documentation "Invalid input to a constructor. INPUT is the offending text."))

(defgeneric css-syntax-error-kind (condition)
  (:method ((c css-syntax-error)) "Invalid CSS"))

(define-condition css-invalid-selector (css-syntax-error) ()
  (:documentation "Selector text outside the 0.1.0 subset, or a bad keyframe selector."))
(defmethod css-syntax-error-kind ((c css-invalid-selector)) "Invalid CSS selector")

(define-condition css-invalid-property (css-syntax-error) ()
  (:documentation "A declaration property that is neither an identifier nor --custom."))
(defmethod css-syntax-error-kind ((c css-invalid-property)) "Invalid CSS property")

(define-condition css-invalid-value (css-syntax-error) ()
  (:documentation "A declaration value that fails the value text rules."))
(defmethod css-syntax-error-kind ((c css-invalid-value)) "Invalid CSS value")

(define-condition css-invalid-identifier (css-syntax-error) ()
  (:documentation "An empty identifier (css-ident, keyframes name, layer segment)."))
(defmethod css-syntax-error-kind ((c css-invalid-identifier)) "Invalid CSS identifier")

(define-condition css-invalid-prelude (css-syntax-error) ()
  (:documentation "A media query, supports condition or import option that fails the text rules."))
(defmethod css-syntax-error-kind ((c css-invalid-prelude)) "Invalid CSS at-rule prelude")

(define-condition css-no-backend (css-error)
  ((source :initarg :source :reader css-no-backend-source :initform nil))
  (:report (lambda (c s)
             (format s "No CSS backend to compile ~S; bind ~S or pass :backend"
                     (css-no-backend-source c) '*css-backend*)))
  (:documentation "COMPILE-STYLESHEET found no backend for SOURCE."))

(defun skip-invalid (&optional condition)
  "Invoke the SKIP-INVALID restart (the constructor returns NIL)."
  (let ((restart (find-restart 'skip-invalid condition)))
    (if restart
        (invoke-restart restart)
        (error 'control-error))))

(defun use-backend (backend &optional condition)
  "Invoke the USE-BACKEND restart of CSS-NO-BACKEND (retry with BACKEND)."
  (let ((restart (find-restart 'use-backend condition)))
    (if restart
        (invoke-restart restart backend)
        (error 'control-error))))

;;; Contract stub marker. Every call site is replaced by the H4h-0b-0
;;; implementation; nothing may call it when the slice is done.

(define-condition css0-pending (error)
  ((what :initarg :what :reader css0-pending-what))
  (:report (lambda (c s)
             (format s "H4h-0b-0 contract stub not implemented: ~S"
                     (css0-pending-what c)))))

(defun %css0-pending (what)
  "Contract stub. Every call site is replaced by the H4h-0b-0 implementation."
  (error 'css0-pending :what what))

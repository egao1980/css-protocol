(in-package #:css-protocol/tests)

;;; compile-stylesheet dispatch and the no-backend condition.

(defclass test-backend (css-backend)
  ((calls :initform nil :accessor test-backend-calls)))

(defmethod backend-compile-stylesheet ((b test-backend) source &key)
  (push source (test-backend-calls b))
  (sheet (r "a" (decl "content" (css-string (princ-to-string source))))))

(deftest compile-stylesheet-identity
  (let ((x (sheet (r "a" (decl "color" "red")))))
    (ok (eq x (compile-stylesheet x)))
    (ok (eq x (let ((*css-backend* (make-instance 'test-backend)))
                (compile-stylesheet x)))
        "a stylesheet never reaches a backend")))

(deftest compile-stylesheet-no-backend
  (let ((*css-backend* nil))
    (ok (signals (compile-stylesheet "a{color:red}") 'css-no-backend))
    (ok (signals (compile-stylesheet '(a :color red)) 'css-error))
    (let ((c (handler-case (compile-stylesheet '(a :color red))
               (css-no-backend (c) c))))
      (ok (equal '(a :color red) (css-no-backend-source c)))
      (ok (search "backend" (string-downcase (princ-to-string c))) "a clear report"))))

(deftest compile-stylesheet-explicit-backend
  (let* ((*css-backend* nil)
         (b (make-instance 'test-backend))
         (x (compile-stylesheet "x" :backend b)))
    (ok (typep x 'stylesheet))
    (ok (equal '("x") (test-backend-calls b)))
    (ok (equal "a{content:\"x\"}" (mini x)))))

(deftest compile-stylesheet-default-backend
  (let* ((b (make-instance 'test-backend))
         (*css-backend* b))
    (ok (equal "a{content:\"y\"}" (mini (compile-stylesheet "y"))))
    (ok (equal '("y") (test-backend-calls b)))))

(deftest compile-stylesheet-explicit-beats-default
  (let* ((default (make-instance 'test-backend))
         (explicit (make-instance 'test-backend))
         (*css-backend* default))
    (compile-stylesheet "z" :backend explicit)
    (ok (null (test-backend-calls default)))
    (ok (equal '("z") (test-backend-calls explicit)))))

(deftest no-backend-use-value
  (let* ((*css-backend* nil)
         (fallback (sheet (r "b" (decl "color" "blue"))))
         (x (handler-bind ((css-no-backend (lambda (c) (use-value fallback c))))
              (compile-stylesheet "a{}"))))
    (ok (eq fallback x))))

(deftest no-backend-use-backend
  (let* ((*css-backend* nil)
         (b (make-instance 'test-backend))
         (x (handler-bind ((css-no-backend (lambda (c) (use-backend b c))))
              (compile-stylesheet "w"))))
    (ok (equal "a{content:\"w\"}" (mini x)))
    (ok (equal '("w") (test-backend-calls b)))))

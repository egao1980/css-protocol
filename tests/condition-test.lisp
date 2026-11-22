(in-package #:css-protocol/tests)

;;; Invalid input -> conditions; USE-VALUE and SKIP-INVALID restarts.

(defmacro with-handler ((type function) &body body)
  `(handler-bind ((,type ,function)) ,@body))

(defun condition-of (thunk)
  (handler-case (progn (funcall thunk) nil)
    (css-error (c) c)))

(deftest invalid-properties
  (dolist (bad '("" "col or" "1x" "a:b" "--" "color;" "-" "co\"lor"))
    (let ((c (condition-of (lambda () (decl bad "red")))))
      (ok (typep c 'css-invalid-property) (format nil "property ~S" bad))
      (when c (ok (equal bad (css-error-input c))))))
  (ok (equal "-webkit-appearance" (declaration-property (decl "-webkit-appearance" "none"))))
  (ok (equal "--é" (declaration-property (decl "--é" "1")))))

(deftest invalid-values
  (dolist (bad (list "" "   " "/* only */" "red; background: url(x)" "red}" "{red" "red{"
                     "rgb(0,0,0" "a)" "[a" "(a]" "'abc" "\"abc"
                     (format nil "\"a~Cb\"" #\Newline)
                     "red !important" "red!" "/* x" "a\\"
                     "</style><script>" "'</style>'" "x</p>"))
    (let ((c (condition-of (lambda () (decl "color" bad)))))
      (ok (typep c 'css-invalid-value) (format nil "value ~S" bad))
      (when c (ok (equal bad (css-error-input c)))))))

(deftest valid-tricky-values
  (ok (equal "\"a;b\"" (declaration-value (decl "content" "\"a;b\""))) "; inside a string")
  (ok (equal "\"!\"" (declaration-value (decl "content" "\"!\""))))
  (ok (equal "\"a\\\"b\"" (declaration-value (decl "content" "\"a\\\"b\""))))
  (ok (equal "{a: b}" (declaration-value (decl "--x" "{a: b}"))) "balanced braces")
  (ok (equal "a\\;b" (declaration-value (decl "--x" "a\\;b"))) "escaped ; is not a separator"))

(deftest invalid-preludes
  (dolist (bad '("" "screen{}a{color:red" "print;" "(max-width: 1px" "</style>" "a}"))
    (let ((c (condition-of (lambda () (make-media-rule bad nil)))))
      (ok (typep c 'css-invalid-prelude) (format nil "media ~S" bad))))
  (ok (signals (make-supports-rule "(display: grid" nil) 'css-invalid-prelude))
  (ok (signals (make-import-rule "a.css" :media "screen;") 'css-invalid-prelude))
  (ok (signals (make-import-rule "a.css" :supports "(a") 'css-invalid-prelude))
  (ok (signals (make-layer-rule '("a" "b") (list (r "p"))) 'css-invalid-prelude)
      "a layer block takes at most one name"))

(deftest invalid-identifiers
  (ok (signals (make-keyframes-rule "" nil) 'css-invalid-identifier))
  (ok (signals (make-layer-rule "") 'css-invalid-identifier))
  (ok (signals (make-layer-rule "a..b") 'css-invalid-identifier))
  (ok (signals (make-layer-rule '("ok" ".x")) 'css-invalid-identifier))
  (ok (signals (make-layer-rule nil) 'css-invalid-identifier) "a statement needs a name"))

(deftest invalid-keyframe-selectors
  (dolist (bad '("50" "x%" "" "from to" "50%,," "-10%"))
    (ok (signals (make-keyframe bad nil) 'css-invalid-selector) (format nil "keyframe ~S" bad))))

(deftest invalid-rule-selector
  (ok (signals (r "a {" (decl "color" "red")) 'css-invalid-selector))
  (ok (signals (make-scope-rule "a {" nil) 'css-invalid-selector))
  (ok (signals (make-scope-rule ".a" nil :end "b;") 'css-invalid-selector)))

(deftest use-value-restart
  (testing "selector"
    (let ((rule (with-handler (css-invalid-selector (lambda (c) (use-value "b" c)))
                  (r "a {" (decl "color" "red")))))
      (ok (equal '("b") (selector-texts rule)))))
  (testing "property"
    (ok (equal "color"
               (declaration-property
                (with-handler (css-invalid-property (lambda (c) (use-value "color" c)))
                  (decl "col or" "red"))))))
  (testing "value"
    (ok (equal "blue"
               (declaration-value
                (with-handler (css-invalid-value (lambda (c) (use-value "blue" c)))
                  (decl "color" "red; x: y"))))))
  (testing "prelude"
    (ok (equal "print"
               (media-rule-query
                (with-handler (css-invalid-prelude (lambda (c) (use-value "print" c)))
                  (make-media-rule "a{" nil))))))
  (testing "identifier"
    (ok (equal "\\31 x"
               (with-handler (css-invalid-identifier (lambda (c) (use-value "1x" c)))
                 (css-ident ""))))))

(deftest use-value-is-revalidated
  (let ((calls 0))
    (let ((d (with-handler (css-invalid-value
                            (lambda (c)
                              (incf calls)
                              (use-value (if (= calls 1) "still; bad" "green") c)))
               (decl "color" "red; x: y"))))
      (ok (= 2 calls))
      (ok (equal "green" (declaration-value d))))))

(deftest skip-invalid-restart
  (testing "constructors return NIL"
    (ok (null (with-handler (css-error #'skip-invalid) (decl "color" "red;"))))
    (ok (null (with-handler (css-error #'skip-invalid) (decl "col or" "red"))))
    (ok (null (with-handler (css-error #'skip-invalid) (r "a {" (decl "color" "red")))))
    (ok (null (with-handler (css-error #'skip-invalid) (parse-selector-list "a {"))))
    (ok (null (with-handler (css-error #'skip-invalid) (make-media-rule "a{" nil))))
    (ok (null (with-handler (css-error #'skip-invalid) (make-keyframe "nope" nil))))
    (ok (null (with-handler (css-error #'skip-invalid) (css-ident "")))))
  (testing "containers drop skipped children"
    (let ((x (with-handler (css-error #'skip-invalid)
               (sheet (r "a {" (decl "color" "red"))
                      (r "p" (decl "color" "red;") (decl "margin" 0))
                      (make-media-rule "a{" (list (r "b")))
                      (make-keyframes-rule "k" (list (make-keyframe "nope" nil)
                                                     (make-keyframe "to" (list (decl "opacity" 1)))))))))
      (ok (equal "p{margin:0}@keyframes k{to{opacity:1}}" (mini x)))))
  (testing "NIL children are dropped by the plain constructors too"
    (ok (equal "a{color:red}" (mini (make-stylesheet (list nil (r "a" nil (decl "color" "red")))))))))

(deftest restarts-are-offered
  (dolist (thunk (list (lambda () (decl "color" "red;"))
                       (lambda () (decl "col or" "red"))
                       (lambda () (parse-selector-list "a {"))
                       (lambda () (make-media-rule "a{" nil))
                       (lambda () (css-ident ""))))
    (let (restarts)
      (handler-case
          (handler-bind ((css-syntax-error
                           (lambda (c)
                             (setf restarts (list (find-restart 'use-value c)
                                                  (find-restart 'skip-invalid c))))))
            (funcall thunk))
        (css-syntax-error () nil))
      (ok (and (first restarts) (second restarts)) "use-value and skip-invalid"))))

(deftest unhandled-is-an-error
  (ok (signals (decl "color" "red;") 'error))
  (ok (signals (decl "color" "red;") 'css-syntax-error))
  (let ((c (condition-of (lambda () (decl "color" "red;")))))
    (ok (search "red;" (princ-to-string c)) "report names the input")))

(deftest type-errors
  (ok (signals (decl "color" :red) 'type-error))
  (ok (signals (decl 'color "red") 'type-error))
  (ok (signals (make-rule "a" (list 42)) 'type-error))
  (ok (signals (make-stylesheet (list "a{}")) 'type-error))
  (ok (signals (make-media-rule "print" (list 42)) 'type-error)))

(deftest skip-invalid-without-restart
  (ok (signals (skip-invalid) 'control-error)))

(in-package #:css-protocol/tests)

;;; scope-stylesheet: :prefix and :scope-rule.

(defun scope-fixture ()
  (sheet (r ":root" (decl "--bg" "#fff"))
         (r "body" (decl "margin" 0))
         (r "a, p > em" (decl "color" "red"))
         (make-media-rule "print" (list (r "a" (decl "color" "black"))))
         (make-font-face-rule (list (decl "font-family" (css-string "X"))
                                    (decl "src" (css-url "x.woff2"))))
         (make-keyframes-rule "spin"
                              (list (make-keyframe "from" (list (decl "opacity" 0)))
                                    (make-keyframe "to" (list (decl "opacity" 1)))))))

(defparameter *fixture-tail*
  "@font-face{font-family:\"X\";src:url(\"x.woff2\")}@keyframes spin{from{opacity:0}to{opacity:1}}")

(deftest scope-prefix
  (let ((x (scope-stylesheet (scope-fixture) "#canvas-1")))
    (ok (equal (concatenate 'string
                            "#canvas-1{--bg:#fff}#canvas-1{margin:0}"
                            "#canvas-1 a,#canvas-1 p>em{color:red}"
                            "@media print{#canvas-1 a{color:black}}"
                            *fixture-tail*)
               (mini x)))))

(deftest scope-prefix-is-default
  (ok (equal (mini (scope-stylesheet (scope-fixture) "#canvas-1"))
             (mini (scope-stylesheet (scope-fixture) "#canvas-1" :method :prefix)))))

(deftest scope-prefix-pretty
  (let ((x (scope-stylesheet (sheet (r "body > p" (decl "margin" 0))) "#c")))
    (ok (equal (lines "#c > p {" "  margin: 0;" "}") (pretty x)))))

(deftest scope-prefix-root-compounds
  (flet ((scoped (selector scope)
           (first (selector-texts (first (stylesheet-rules
                                          (scope-stylesheet (sheet (r selector (decl "color" "red")))
                                                            scope)))))))
    (ok (equal "#c:not([data-theme=\"light\"])" (scoped ":root:not([data-theme=\"light\"])" "#c")))
    (ok (equal "#c" (scoped "html" "#c")))
    (ok (equal "#c.dark p" (scoped "html.dark p" "#c")))
    (ok (equal "#c *" (scoped "*" "#c")))
    (ok (equal ".a .b a" (scoped "a" ".a .b")))
    (ok (equal ".a .b" (scoped ":root" ".a .b")))
    (ok (equal ".a .b.x" (scoped ":root.x" ".a .b")))))

(deftest scope-rule-method
  (let ((x (scope-stylesheet (scope-fixture) "#canvas-1" :method :scope-rule)))
    (ok (equal (concatenate 'string
                            "@scope (#canvas-1){:scope{--bg:#fff}:scope{margin:0}"
                            "a,p>em{color:red}@media print{a{color:black}}}"
                            *fixture-tail*)
               (mini x)))
    (ok (typep (first (stylesheet-rules x)) 'scope-rule))))

(deftest scope-rule-root-compounds
  (let ((x (scope-stylesheet (sheet (r ":root:not([data-theme=\"light\"])" (decl "--bg" "#000"))
                                    (r "html.dark p" (decl "color" "red")))
                             "#c" :method :scope-rule)))
    (ok (equal "@scope (#c){:scope:not([data-theme=\"light\"]){--bg:#000}:scope.dark p{color:red}}"
               (mini x)))))

(defun runs-fixture ()
  (sheet (make-import-rule "t.css")
         (r "a" (decl "color" "red"))
         (make-font-face-rule (list (decl "font-family" (css-string "X"))))
         (r "b" (decl "color" "blue"))
         (make-layer-rule "base")
         (make-layer-rule "base" (list (r "c" (decl "margin" 0))))
         (make-scope-rule ".card" (list (r "img" (decl "border" 0))))))

(deftest scope-rule-runs-keep-order
  (ok (equal (concatenate 'string
                          "@import url(\"t.css\");"
                          "@scope (#c){a{color:red}}"
                          "@font-face{font-family:\"X\"}"
                          "@scope (#c){b{color:blue}}"
                          "@layer base;"
                          "@layer base{@scope (#c){c{margin:0}}}"
                          "@scope (#c){@scope (.card){img{border:0}}}")
             (mini (scope-stylesheet (runs-fixture) "#c" :method :scope-rule)))))

(deftest scope-prefix-at-rules
  (ok (equal (concatenate 'string
                          "@import url(\"t.css\");"
                          "#c a{color:red}"
                          "@font-face{font-family:\"X\"}"
                          "#c b{color:blue}"
                          "@layer base;"
                          "@layer base{#c c{margin:0}}"
                          "@scope (#c .card){img{border:0}}")
             (mini (scope-stylesheet (runs-fixture) "#c")))))

(deftest scope-supports-recursion
  (ok (equal "@supports (display: grid){@media print{#c main{display:grid}}}"
             (mini (scope-stylesheet
                    (sheet (make-supports-rule "(display: grid)"
                                               (list (make-media-rule "print"
                                                                      (list (r "main" (decl "display" "grid")))))))
                    "#c")))))

(deftest scope-does-not-mutate
  (let* ((x (scope-fixture))
         (before (mini x)))
    (scope-stylesheet x "#c")
    (scope-stylesheet x "#c" :method :scope-rule)
    (ok (equal before (mini x)))))

(deftest scope-invalid-arguments
  (ok (signals (scope-stylesheet (scope-fixture) "a {") 'css-invalid-selector))
  (ok (signals (scope-stylesheet (scope-fixture) "#c" :method :bogus) 'type-error)))

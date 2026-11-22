(in-package #:css-protocol/tests)

;;; Golden strings for every construct, pretty and minified.

(deftest serialize-empty-stylesheet
  (ok (equal "" (pretty (sheet))))
  (ok (equal "" (mini (sheet))))
  (ok (null (stylesheet-rules (sheet)))))

(deftest serialize-simple-rule
  (let ((x (sheet (r "a" (decl "color" "red")))))
    (ok (equal (lines "a {" "  color: red;" "}") (pretty x)))
    (ok (equal "a{color:red}" (mini x)))))

(deftest serialize-selector-list-and-important
  (let ((x (sheet (r "a, b > c"
                     (decl "color" "red")
                     (decl "margin" "0 auto" :important t)))))
    (ok (equal (lines "a, b > c {"
                      "  color: red;"
                      "  margin: 0 auto !important;"
                      "}")
               (pretty x)))
    (ok (equal "a,b>c{color:red;margin:0 auto!important}" (mini x)))))

(deftest serialize-combinators
  (let ((x (sheet (r "a b,a>b,a+b,a~b" (decl "color" "red")))))
    (ok (equal (lines "a b, a > b, a + b, a ~ b {" "  color: red;" "}")
               (pretty x)))
    (ok (equal "a b,a>b,a+b,a~b{color:red}" (mini x)))))

(deftest serialize-attribute-and-pseudo
  (let ((x (sheet (r "input[type=\"text\"]:focus::placeholder, a:not(.b)"
                     (decl "opacity" 0.5)))))
    (ok (equal (lines "input[type=\"text\"]:focus::placeholder, a:not(.b) {"
                      "  opacity: 0.5;"
                      "}")
               (pretty x)))
    (ok (equal "input[type=\"text\"]:focus::placeholder,a:not(.b){opacity:0.5}"
               (mini x)))))

(deftest serialize-empty-rule
  (let ((x (sheet (r "a"))))
    (ok (equal (lines "a {}") (pretty x)))
    (ok (equal "a{}" (mini x)))))

(deftest serialize-two-rules-blank-line
  (let ((x (sheet (r "a" (decl "color" "red"))
                  (r "b" (decl "color" "blue")))))
    (ok (equal (lines "a {" "  color: red;" "}"
                      ""
                      "b {" "  color: blue;" "}")
               (pretty x)))
    (ok (equal "a{color:red}b{color:blue}" (mini x)))))

(deftest serialize-custom-properties
  (let ((x (sheet (r ":root"
                     (decl "--Main-BG" "#fff")
                     (decl "--gap" "calc(1rem + 2px)")
                     (decl "background" (css-var "Main-BG" "white"))))))
    (ok (equal (lines ":root {"
                      "  --Main-BG: #fff;"
                      "  --gap: calc(1rem + 2px);"
                      "  background: var(--Main-BG, white);"
                      "}")
               (pretty x)))
    (ok (equal ":root{--Main-BG:#fff;--gap:calc(1rem + 2px);background:var(--Main-BG, white)}"
               (mini x)))))

(deftest serialize-media
  (let ((x (sheet (make-media-rule "print"
                                   (list (r "a" (decl "color" "black"))
                                         (r "p" (decl "margin" 0)))))))
    (ok (equal (lines "@media print {"
                      "  a {"
                      "    color: black;"
                      "  }"
                      ""
                      "  p {"
                      "    margin: 0;"
                      "  }"
                      "}")
               (pretty x)))
    (ok (equal "@media print{a{color:black}p{margin:0}}" (mini x)))
    (ok (equal "media" (at-rule-name (first (stylesheet-rules x)))))
    (ok (equal "print" (media-rule-query (first (stylesheet-rules x)))))))

(deftest serialize-media-query-normalised
  (let ((m (make-media-rule "screen   and
 (max-width:  400px)" nil)))
    (ok (equal "screen and (max-width: 400px)" (media-rule-query m)))
    (ok (equal (lines "@media screen and (max-width: 400px) {}") (pretty m)))
    (ok (equal "@media screen and (max-width: 400px){}" (mini m)))))

(deftest serialize-supports-with-nested-media
  (let ((x (sheet (make-supports-rule
                   "(display: grid)"
                   (list (make-media-rule "(min-width: 40em)"
                                          (list (r "main" (decl "display" "grid")))))))))
    (ok (equal (lines "@supports (display: grid) {"
                      "  @media (min-width: 40em) {"
                      "    main {"
                      "      display: grid;"
                      "    }"
                      "  }"
                      "}")
               (pretty x)))
    (ok (equal "@supports (display: grid){@media (min-width: 40em){main{display:grid}}}"
               (mini x)))
    (ok (equal "supports" (at-rule-name (first (stylesheet-rules x)))))))

(deftest serialize-layer-statement
  (let ((l (make-layer-rule '("base" "components.forms"))))
    (ok (layer-rule-statement-p l))
    (ok (equal '("base" "components.forms") (layer-rule-names l)))
    (ok (equal (lines "@layer base, components.forms;") (pretty l)))
    (ok (equal "@layer base,components.forms;" (mini l))))
  (ok (equal (lines "@layer reset;") (pretty (make-layer-rule "reset")))))

(deftest serialize-layer-block
  (let ((l (make-layer-rule "base" (list (r "a" (decl "color" "red"))))))
    (ng (layer-rule-statement-p l))
    (ok (equal (lines "@layer base {" "  a {" "    color: red;" "  }" "}") (pretty l)))
    (ok (equal "@layer base{a{color:red}}" (mini l))))
  (let ((anon (make-layer-rule nil (list (r "a" (decl "color" "red"))))))
    (ok (equal (lines "@layer {" "  a {" "    color: red;" "  }" "}") (pretty anon)))
    (ok (equal "@layer{a{color:red}}" (mini anon))))
  (ok (equal (lines "@layer base {}") (pretty (make-layer-rule "base" nil)))))

(deftest serialize-font-face
  (let ((f (make-font-face-rule
            (list (decl "font-family" (css-string "Engels Sans"))
                  (decl "src" (format nil "~A format(\"woff2\")" (css-url "e.woff2")))))))
    (ok (equal (lines "@font-face {"
                      "  font-family: \"Engels Sans\";"
                      "  src: url(\"e.woff2\") format(\"woff2\");"
                      "}")
               (pretty f)))
    (ok (equal "@font-face{font-family:\"Engels Sans\";src:url(\"e.woff2\") format(\"woff2\")}"
               (mini f)))
    (ok (= 2 (length (rule-declarations f))))
    (ok (equal "font-face" (at-rule-name f)))))

(deftest serialize-keyframes
  (let ((k (make-keyframes-rule
            "spin"
            (list (make-keyframe "from" (list (decl "transform" "rotate(0deg)")))
                  (make-keyframe "50%, 75.5%" (list (decl "opacity" 0.5)))
                  (make-keyframe '("TO") (list (decl "transform" "rotate(360deg)")))))))
    (ok (equal (lines "@keyframes spin {"
                      "  from {"
                      "    transform: rotate(0deg);"
                      "  }"
                      ""
                      "  50%, 75.5% {"
                      "    opacity: 0.5;"
                      "  }"
                      ""
                      "  to {"
                      "    transform: rotate(360deg);"
                      "  }"
                      "}")
               (pretty k)))
    (ok (equal "@keyframes spin{from{transform:rotate(0deg)}50%,75.5%{opacity:0.5}to{transform:rotate(360deg)}}"
               (mini k)))
    (ok (equal '("50%" "75.5%") (keyframe-selectors (second (keyframes-rule-keyframes k)))))
    (ok (equal '("to") (keyframe-selectors (third (keyframes-rule-keyframes k)))))
    (ok (equal "spin" (keyframes-rule-name k)))))

(deftest serialize-import
  (let ((plain (make-import-rule "theme.css"))
        (full (make-import-rule "theme.css" :layer "base" :supports "display: grid"
                                            :media "screen and (min-width: 40em)"))
        (anon (make-import-rule "theme.css" :layer t)))
    (ok (equal (lines "@import url(\"theme.css\");") (pretty plain)))
    (ok (equal "@import url(\"theme.css\");" (mini plain)))
    (ok (equal (lines "@import url(\"theme.css\") layer(base) supports(display: grid) screen and (min-width: 40em);")
               (pretty full)))
    (ok (equal "@import url(\"theme.css\") layer(base) supports(display: grid) screen and (min-width: 40em);"
               (mini full)))
    (ok (equal "@import url(\"theme.css\") layer;" (mini anon)))
    (ok (equal "theme.css" (import-rule-url full)))
    (ok (equal "base" (import-rule-layer full)))
    (ok (eq t (import-rule-layer anon)))))

(deftest serialize-import-before-rules
  (let ((x (sheet (make-import-rule "a.css") (r "a" (decl "color" "red")))))
    (ok (equal (lines "@import url(\"a.css\");" "" "a {" "  color: red;" "}") (pretty x)))
    (ok (equal "@import url(\"a.css\");a{color:red}" (mini x)))))

(deftest serialize-scope-rule
  (let ((s (make-scope-rule ".card > .body" (list (r "img" (decl "border" 0)))
                            :end ".content")))
    (ok (equal (lines "@scope (.card > .body) to (.content) {"
                      "  img {"
                      "    border: 0;"
                      "  }"
                      "}")
               (pretty s)))
    (ok (equal "@scope (.card>.body) to (.content){img{border:0}}" (mini s)))
    (ok (equal "scope" (at-rule-name s))))
  (let ((s (make-scope-rule "#c" (list (r ":scope" (decl "margin" 0))))))
    (ok (equal "@scope (#c){:scope{margin:0}}" (mini s)))
    (ok (null (scope-rule-end s)))))

(deftest serialize-single-node
  (testing "a non-stylesheet node prints as a one-item sheet"
    (ok (equal (lines "a {" "  color: red;" "}") (pretty (r "a" (decl "color" "red")))))
    (ok (equal "a{color:red}" (mini (r "a" (decl "color" "red")))))))

(deftest serialize-stream-argument
  (let ((x (sheet (r "a" (decl "color" "red")))))
    (ok (equal "a{color:red}"
               (with-output-to-string (s)
                 (ok (null (write-stylesheet x :stream s :pretty nil))))))
    (ok (equal (lines "a {" "  color: red;" "}")
               (with-output-to-string (*standard-output*)
                 (write-stylesheet x :stream t))))))

(deftest serialize-numbers
  (flet ((v (x) (declaration-value (decl "margin" x))))
    (ok (equal "0" (v 0)))
    (ok (equal "3" (v 3)))
    (ok (equal "-12" (v -12)))
    (ok (equal "2" (v 2.0)))
    (ok (equal "1.5" (v 1.5)))
    (ok (equal "-0.5" (v -0.5d0)))
    (ok (equal "0.25" (v 1/4)))))

(deftest serialize-value-normalisation
  (flet ((v (x) (declaration-value (decl "margin" x))))
    (ok (equal "0 auto" (v "0   auto")))
    (ok (equal "red" (v "  red  ")))
    (ok (equal "a b" (v (format nil "a~C~Cb" #\Newline #\Tab))))
    (ok (equal "a b" (v "a /* comment */ b")))
    (ok (equal "\"a  b\"" (v "\"a  b\"")))
    (ok (equal "'x /* y */'" (v "'x /* y */'")))
    (ok (equal "calc( 1px + 2px )" (v "calc( 1px  +  2px )")) "only runs collapse")))

(deftest serialize-property-case
  (ok (equal "color" (declaration-property (decl "COLOR" "red"))))
  (ok (equal "--Main-BG" (declaration-property (decl "--Main-BG" "red"))))
  (ok (custom-property-p (decl "--x" "1")))
  (ok (custom-property-p "--x"))
  (ng (custom-property-p (decl "color" "red")))
  (ok (eq t (declaration-important-p (decl "color" "red" :important :yes))))
  (ng (declaration-important-p (decl "color" "red"))))

(deftest merge-stylesheets-appends
  (let* ((a (sheet (r "a" (decl "color" "red"))))
         (b (sheet (r "b" (decl "color" "blue"))))
         (m (merge-stylesheets a b)))
    (ok (equal "a{color:red}b{color:blue}" (mini m)))
    (ok (= 1 (length (stylesheet-rules a))) "inputs unchanged")
    (ok (equal "" (mini (merge-stylesheets))))))

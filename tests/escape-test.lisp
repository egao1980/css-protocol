(in-package #:css-protocol/tests)

;;; CSSOM identifier / string serialization, url(), and hostile input.

(defun chars (&rest parts)
  "Concatenate strings and characters / char codes into one string."
  (with-output-to-string (s)
    (dolist (p parts)
      (etypecase p
        (string (write-string p s))
        (character (write-char p s))
        (integer (write-char (code-char p) s))))))

(deftest ident-plain
  (ok (equal "foo" (css-ident "foo")))
  (ok (equal "_x-y9" (css-ident "_x-y9")))
  (ok (equal "--x" (css-ident "--x")))
  (ok (equal "é" (css-ident "é")) "non-ASCII stays"))

(deftest ident-digits-and-dashes
  (ok (equal "\\31 st" (css-ident "1st")))
  (ok (equal "-\\31 x" (css-ident "-1x")))
  (ok (equal "\\-" (css-ident "-")))
  (ok (equal "a1" (css-ident "a1"))))

(deftest ident-punctuation
  (ok (equal "a\\ b" (css-ident "a b")))
  (ok (equal "a\\.b\\#c" (css-ident "a.b#c")))
  (ok (equal "a\\/b" (css-ident "a/b")))
  (ok (equal "a\\\"b" (css-ident "a\"b")))
  (ok (equal "\\<\\/style\\>" (css-ident "</style>")))
  (ok (equal "\\31 \\ spin" (css-ident "1 spin"))))

(deftest ident-controls
  (ok (equal "a\\9 b" (css-ident (chars "a" #\Tab "b"))))
  (ok (equal "a\\a b" (css-ident (chars "a" #\Newline "b"))))
  (ok (equal "a\\7f " (css-ident (chars "a" 127))))
  (ok (equal (chars "a" #xFFFD) (css-ident (chars "a" 0))) "NUL becomes U+FFFD"))

(deftest string-escaping
  (ok (equal "\"abc\"" (css-string "abc")))
  (ok (equal "\"\"" (css-string "")))
  (ok (equal "\"a\\\"b\"" (css-string "a\"b")))
  (ok (equal "\"a\\\\b\"" (css-string "a\\b")))
  (ok (equal "\"'single'\"" (css-string "'single'")))
  (ok (equal "\"a\\a b\"" (css-string (chars "a" #\Newline "b"))))
  (ok (equal "\"a\\d b\"" (css-string (chars "a" #\Return "b"))))
  (ok (equal (chars "\"" #xFFFD "\"") (css-string (chars 0))))
  (ok (equal "\"é\"" (css-string "é"))))

(deftest string-escapes-html-end-tag
  (ok (equal "\"\\3c /style>\"" (css-string "</style>")))
  (ok (equal "\"a\\3c b\"" (css-string "a<b")))
  (ok (null (search "<" (css-string "<<</style><script>")))))

(deftest url-quoting
  (ok (equal "url(\"x.png\")" (css-url "x.png")))
  (ok (equal "url(\"a b\\\").png\")" (css-url "a b\").png")))
  (ok (equal "url(\"\\3c /style>\")" (css-url "</style>")))
  (ok (equal "url(\"data:image/svg+xml;utf8,\\3c svg/>\")"
             (css-url "data:image/svg+xml;utf8,<svg/>"))))

(deftest var-references
  (ok (equal "var(--bg)" (css-var "bg")))
  (ok (equal "var(--bg)" (css-var "--bg")))
  (ok (equal "var(--bg, #fff)" (css-var "bg" "#fff")))
  (ok (equal "var(--gap, 0)" (css-var "gap" 0)))
  (ok (equal "var(--a\\ b)" (css-var "a b"))))

(deftest hostile-values-never-emit-end-tag
  (testing "escaped strings and urls go through declarations unharmed"
    (let ((out (mini (sheet (r "a::before"
                               (decl "content" (css-string "</style><script>x</script>"))
                               (decl "background" (css-url "</style>")))))))
      (ok (null (search "</" out)))
      (ok (search "content:\"\\3c /style>\\3c script>x\\3c /script>\"" out))))
  (testing "escaped identifiers in selectors"
    (let ((out (mini (sheet (r (format nil ".~A" (css-ident "x</style>"))
                               (decl "color" "red"))))))
      (ok (null (search "</" out)))
      (ok (equal ".x\\<\\/style\\>{color:red}" out))))
  (testing "keyframes names are escaped"
    (ok (equal "@keyframes \\31 \\ spin{from{opacity:0}}"
               (mini (make-keyframes-rule "1 spin"
                                          (list (make-keyframe "from" (list (decl "opacity" 0))))))))))

(deftest import-url-escaped
  (ok (equal "@import url(\"a\\\".css\");" (mini (make-import-rule "a\".css"))))
  (ok (null (search "</" (mini (make-import-rule "</style>"))))))

(deftest ident-empty-is-invalid
  (ok (signals (css-ident "") 'css-invalid-identifier))
  (ok (signals (css-ident 42) 'type-error)))

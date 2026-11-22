(in-package #:css-protocol/tests)

;;; Custom-property themes with prefers-color-scheme.

(defparameter *light* '(("bg" . "#fff") ("fg" . "#111")))
(defparameter *dark* '(("bg" . "#111") ("fg" . "#eee")))

(deftest theme-keys-normalised
  (let ((theme (make-css-theme :light '(("bg" . "#fff") ("--fg" . "#111") (:accent-color . "#07f"))
                               :dark '(:bg "#000" :fg 0))))
    (ok (equal '(("--bg" . "#fff") ("--fg" . "#111") ("--accent-color" . "#07f"))
               (css-theme-light theme)))
    (ok (equal '(("--bg" . "#000") ("--fg" . "0")) (css-theme-dark theme)) "plists and reals")
    (ok (equal ":root" (css-theme-selector theme)))
    (ok (null (css-theme-toggle-attribute theme)))))

(deftest theme-light-only
  (let ((x (css-theme-stylesheet (make-css-theme :light *light*))))
    (ok (equal (lines ":root {" "  --bg: #fff;" "  --fg: #111;" "}") (pretty x)))
    (ok (equal ":root{--bg:#fff;--fg:#111}" (mini x)))))

(deftest theme-prefers-color-scheme
  (let ((x (css-theme-stylesheet (make-css-theme :light *light* :dark *dark*))))
    (ok (equal (lines ":root {"
                      "  color-scheme: light dark;"
                      "  --bg: #fff;"
                      "  --fg: #111;"
                      "}"
                      ""
                      "@media (prefers-color-scheme: dark) {"
                      "  :root {"
                      "    --bg: #111;"
                      "    --fg: #eee;"
                      "  }"
                      "}")
               (pretty x)))
    (ok (equal ":root{color-scheme:light dark;--bg:#fff;--fg:#111}@media (prefers-color-scheme: dark){:root{--bg:#111;--fg:#eee}}"
               (mini x)))
    (let ((media (second (stylesheet-rules x))))
      (ok (typep media 'media-rule))
      (ok (equal "(prefers-color-scheme: dark)" (media-rule-query media)))
      (ok (every #'custom-property-p (rule-declarations (first (at-rule-rules media))))))))

(deftest theme-toggle-attribute
  (let ((x (css-theme-stylesheet (make-css-theme :light *light* :dark *dark*
                                                 :toggle-attribute "data-theme"))))
    (ok (equal (lines ":root {"
                      "  color-scheme: light dark;"
                      "  --bg: #fff;"
                      "  --fg: #111;"
                      "}"
                      ""
                      "@media (prefers-color-scheme: dark) {"
                      "  :root:not([data-theme=\"light\"]) {"
                      "    --bg: #111;"
                      "    --fg: #eee;"
                      "  }"
                      "}"
                      ""
                      ":root[data-theme=\"dark\"] {"
                      "  color-scheme: dark;"
                      "  --bg: #111;"
                      "  --fg: #eee;"
                      "}"
                      ""
                      ":root[data-theme=\"light\"] {"
                      "  color-scheme: light;"
                      "}")
               (pretty x)))
    (ok (= 4 (length (stylesheet-rules x))))))

(deftest theme-custom-selector
  (let ((x (css-theme-stylesheet (make-css-theme :light '(("bg" . "#fff"))
                                                 :dark '(("bg" . "#000"))
                                                 :selector ".canvas"))))
    (ok (equal ".canvas{color-scheme:light dark;--bg:#fff}@media (prefers-color-scheme: dark){.canvas{--bg:#000}}"
               (mini x)))))

(deftest theme-composes-with-rules
  (let* ((theme (css-theme-stylesheet (make-css-theme :light '(("bg" . "#fff")))))
         (x (merge-stylesheets theme (sheet (r "body" (decl "background" (css-var "bg")))))))
    (ok (equal ":root{--bg:#fff}body{background:var(--bg)}" (mini x)))))

(deftest theme-invalid-value
  (ok (signals (make-css-theme :light '(("bg" . "red; color: blue"))) 'css-invalid-value))
  (ok (equal '(("--bg" . "red"))
             (css-theme-light
              (handler-bind ((css-invalid-value (lambda (c) (use-value "red" c))))
                (make-css-theme :light '(("bg" . "red; color: blue"))))))))

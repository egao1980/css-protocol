(in-package #:css-protocol/tests)

;;; used-rules against html-protocol trees.

(defun page-fragment ()
  "A canvas fragment: no html / body element."
  (el "main" '("id" "top")
      (el "div" '("class" "card  wide")
          (el "p" '("class" "a") (text-node "one"))
          (el "p" '("class" "b") (text-node "two")))
      (el "ul" nil
          (el "li" (list "class" (format nil "x~Cy" #\Tab)) (text-node "item")))
      (el "a" '("href" "#top") (text-node "up"))))

(defun kept-p (selector &optional (tree (page-fragment)))
  "True when a one-rule sheet with SELECTOR survives used-rules."
  (not (null (stylesheet-rules
              (used-rules (sheet (r selector (decl "color" "red"))) tree)))))

(deftest used-rules-types
  (ok (kept-p "p"))
  (ok (kept-p "DIV") "type compares ignoring case")
  (ng (kept-p "span"))
  (ng (kept-p "table")))

(deftest used-rules-classes-and-ids
  (ok (kept-p ".card"))
  (ok (kept-p ".wide") "class attribute split on whitespace")
  (ok (kept-p "div.card.wide"))
  (ng (kept-p ".missing"))
  (ng (kept-p ".Card") "classes are case-sensitive")
  (ok (kept-p "#top"))
  (ng (kept-p "#bottom"))
  (ok (kept-p "main#top"))
  (ng (kept-p "div#top") "type and id must be on one element"))

(deftest used-rules-single-element
  (ok (kept-p "p.a"))
  (ng (kept-p "p.a.b") "no single p has both classes")
  (ng (kept-p "div.a")))

(deftest used-rules-tab-separated-class
  (ok (kept-p ".y"))
  (ok (kept-p "li.x")))

(deftest used-rules-complex
  (ok (kept-p "div p"))
  (ok (kept-p "main > div > p"))
  (ng (kept-p "div span") "every compound must be satisfiable")
  (ok (kept-p "p ~ a") "combinators are ignored (conservative)")
  (ok (kept-p "ul > li::before")))

(deftest used-rules-conservative-keeps
  (testing "pseudo-classes, pseudo-elements, attributes count as satisfied"
    (ok (kept-p "a:hover"))
    (ok (kept-p "a[href^=\"#\"]"))
    (ok (kept-p "[data-x]"))
    (ok (kept-p ":is(.zzz)"))
    (ok (kept-p ":not(.zzz)"))
    (ok (kept-p "::selection"))
    (ng (kept-p "button:hover") "the type part still has to exist"))
  (testing "html, body, :root and * are always satisfied"
    (ok (kept-p "html"))
    (ok (kept-p "body"))
    (ok (kept-p ":root"))
    (ok (kept-p "body.dark"))
    (ok (kept-p ":root:not([data-theme=\"light\"])"))
    (ok (kept-p "*"))
    (ok (kept-p "html body p")))
  (testing "html, body do not rescue another missing compound"
    (ng (kept-p "body table"))))

(deftest used-rules-keeps-rule-unchanged
  (let* ((rule (r ".missing, .card" (decl "color" "red")))
         (out (used-rules (sheet rule) (page-fragment))))
    (ok (eq rule (first (stylesheet-rules out))) "whole selector list kept, same object")
    (ok (equal ".missing,.card{color:red}" (mini out)))))

(deftest used-rules-grouping-rules
  (let* ((x (sheet (make-media-rule "print" (list (r "span" (decl "color" "red"))))
                   (make-media-rule "screen" (list (r "span" (decl "color" "red"))
                                                   (r "p" (decl "color" "blue"))))
                   (make-supports-rule "(display: grid)"
                                       (list (make-media-rule "print"
                                                              (list (r "table" (decl "margin" 0))))))
                   (make-layer-rule "base" (list (r "table" (decl "margin" 0))))
                   (make-scope-rule ".card" (list (r "p" (decl "margin" 0))))))
         (out (used-rules x (page-fragment))))
    (ok (equal "@media screen{p{color:blue}}@scope (.card){p{margin:0}}" (mini out)))))

(deftest used-rules-always-kept-at-rules
  (let* ((x (sheet (make-import-rule "t.css")
                   (make-layer-rule '("base" "theme"))
                   (make-font-face-rule (list (decl "font-family" (css-string "X"))))
                   (make-keyframes-rule "spin"
                                        (list (make-keyframe "to" (list (decl "opacity" 1)))))
                   (r "table" (decl "margin" 0))))
         (out (used-rules x (page-fragment))))
    (ok (equal "@import url(\"t.css\");@layer base,theme;@font-face{font-family:\"X\"}@keyframes spin{to{opacity:1}}"
               (mini out)))))

(deftest used-rules-tree-kinds
  (let ((x (sheet (r "p" (decl "color" "red")) (r "span" (decl "color" "blue")))))
    (ok (equal "p{color:red}"
               (mini (used-rules x (make-html-document :children (list (page-fragment)))))))
    (ok (equal "p{color:red}" (mini (used-rules x (page-fragment)))))
    (ok (equal "p{color:red}" (mini (used-rules x (list (el "p" nil) (text-node "x"))))))
    (ok (equal "" (mini (used-rules x nil))) "an empty tree keeps nothing selectable")))

(deftest used-rules-does-not-mutate
  (let* ((x (sheet (r "span" (decl "color" "red")) (r "p" (decl "color" "blue"))))
         (before (mini x)))
    (used-rules x (page-fragment))
    (ok (equal before (mini x)))))

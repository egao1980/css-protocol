(in-package #:css-protocol/tests)

;;; Selector parsing: structure, canonical text, refused forms.

(defun one-selector (text)
  (let ((list (parse-selector-list text)))
    (ok (= 1 (length list)) text)
    (first list)))

(defun canon (text &key (pretty t))
  (mapcar (lambda (s) (selector-text s :pretty pretty)) (parse-selector-list text)))

(deftest selector-structure
  (let* ((s (one-selector "DIV.a.b#x > p"))
         (parts (selector-compounds s)))
    (ok (= 3 (length parts)))
    (ok (typep (first parts) 'compound-selector))
    (ok (eq :child (second parts)))
    (ok (equal "div" (compound-type (first parts))))
    (ok (equal '("a" "b") (compound-classes (first parts))))
    (ok (equal '("x") (compound-ids (first parts))))
    (ok (null (compound-others (first parts))))
    (ok (equal "p" (compound-type (third parts))))))

(deftest selector-combinator-keywords
  (ok (equal '(:descendant :child :next-sibling :subsequent-sibling)
             (remove-if-not #'keywordp
                            (selector-compounds (one-selector "a b > c + d ~ e"))))))

(deftest selector-others-verbatim
  (let ((c (first (selector-compounds
                   (one-selector "input[type=\"text\"]:focus:not(.a, .b)::placeholder")))))
    (ok (equal "input" (compound-type c)))
    (ok (equal '("[type=\"text\"]" ":focus" ":not(.a, .b)" "::placeholder")
               (compound-others c)))))

(deftest selector-universal-and-typeless
  (let ((star (first (selector-compounds (one-selector "*.x"))))
        (bare (first (selector-compounds (one-selector ".x")))))
    (ok (equal "*" (compound-type star)))
    (ok (null (compound-type bare)))
    (ok (equal '("x") (compound-classes bare)))))

(deftest selector-escapes-unescaped-in-parts
  (ok (equal '("1st") (compound-classes (first (selector-compounds (one-selector ".\\31 st"))))))
  (ok (equal '("a.b") (compound-ids (first (selector-compounds (one-selector "#a\\.b"))))))
  (ok (equal '("x y") (compound-classes (first (selector-compounds (one-selector ".x\\ y")))))))

(deftest selector-canonical-text
  (ok (equal '("a > b") (canon "a  >  b")))
  (ok (equal '("a>b") (canon "a  >  b" :pretty nil)))
  (ok (equal '("a b") (canon (format nil "a~C~Cb" #\Newline #\Tab))))
  (ok (equal '("a + b") (canon "a+b")))
  (ok (equal '("a~b") (canon "a ~ b" :pretty nil)))
  (ok (equal '("div.x#y") (canon "DIV.x#y")) "source order after the type")
  (ok (equal '("#y.x") (canon "#y.x")))
  (ok (equal '("*") (canon "*")))
  (ok (equal '(".\\31 st") (canon ".\\31 st")))
  (ok (equal '("#a\\.b") (canon "#a\\.b")))
  (ok (equal '("a") (canon "  a  ")))
  (ok (equal '("a:hover::before") (canon "a:hover::before"))))

(deftest selector-lists
  (ok (equal '("a" "b" "c") (canon "a,b , c")))
  (ok (equal '("a, b, c") (list (format nil "~{~A~^, ~}" (canon "a,b , c")))))
  (ok (= 2 (length (parse-selector-list "a:not(.x, .y), b"))) "commas inside parens do not split")
  (ok (= 1 (length (parse-selector-list "[title=\"a,b\"]"))) "commas inside strings do not split"))

(deftest make-rule-selector-arguments
  (ok (equal '("a" "b") (selector-texts (r "a, b"))))
  (ok (equal '("a" "b" "c") (selector-texts (r (list "a, b" "c")))))
  (let ((s (first (parse-selector-list "p > em"))))
    (ok (eq s (first (rule-selectors (r s)))))
    (ok (equal '("p>em") (selector-texts (r s) :pretty nil)))))

(deftest selector-refused-forms
  (dolist (bad '("" "   " "a {" "a}" "a;b" "a," ",a" "a,,b" "a >" "> a" "a >> b"
                 "a + > b" "a[href" "a[]" "a:not(.x" ".1x" "#" "." "a..b"
                 "svg|rect" "&.x" "a < b" "a</style>" "[title=\"x]"))
    (let ((condition (handler-case (progn (parse-selector-list bad) nil)
                       (css-invalid-selector (c) c))))
      (ok condition (format nil "refused: ~S" bad))
      (when condition
        (ok (equal bad (css-error-input condition)) (format nil "input kept: ~S" bad))))))

(deftest selector-invalid-is-syntax-error
  (ok (signals (parse-selector-list "a {") 'css-syntax-error))
  (ok (signals (parse-selector-list "a {") 'css-error))
  (ok (signals (parse-selector-list 42) 'type-error)))

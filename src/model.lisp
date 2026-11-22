;;;; Stylesheet model. Classes, slots and readers are GIVEN; the validating
;;;; constructors follow the rules below.
;;;;
;;;; Constructor rules (brief "Validation"):
;;;;
;;;; MAKE-DECLARATION property value &key important
;;;;   PROPERTY: a string. "--" + one or more of [A-Za-z0-9_-] or chars >= U+0080
;;;;     is a custom property, case kept. Otherwise it must match
;;;;     -?[A-Za-z_][A-Za-z0-9_-]* and is lowercased. Else CSS-INVALID-PROPERTY.
;;;;     A non-string property is a TYPE-ERROR.
;;;;   VALUE: a string or a REAL, else TYPE-ERROR. Reals: integers in decimal;
;;;;     ratios and floats in plain decimal (no exponent), trailing zeros and a
;;;;     trailing "." dropped: 2.0 -> "2", 1.5 -> "1.5", 1/4 -> "0.25",
;;;;     -0.5 -> "-0.5". Strings are normalised: /* comments */ outside quoted
;;;;     strings become one space; runs of whitespace (space, tab, CR, LF, FF)
;;;;     outside strings become one space; the result is trimmed. Quoted strings
;;;;     ("..." or '...') are copied as written, backslash escapes included.
;;;;     Outside strings a backslash escapes the next character too ("a\;b"
;;;;     has no separator). Braces may appear when balanced ("--x: {a: b}").
;;;;     CSS-INVALID-VALUE when the normalised text is empty, or has an
;;;;     unterminated string, comment or escape (a trailing "\"), a raw newline
;;;;     inside a string, unbalanced or mismatched () [] {}, a ";" or "!" outside
;;;;     strings, or the two characters "</" anywhere (strings included).
;;;;   IMPORTANT: generalized boolean, stored as T / NIL.
;;;;
;;;; MAKE-RULE selectors declarations
;;;;   SELECTORS: a string (comma list allowed), a SELECTOR, or a list of
;;;;   those; strings go through PARSE-SELECTOR-LIST; the result is the flat
;;;;   list of SELECTOR objects in order. DECLARATIONS: a list of
;;;;   CSS-DECLARATION or NIL (NILs dropped); anything else is a TYPE-ERROR.
;;;;   If the selector list is skipped (SKIP-INVALID) the rule is NIL.
;;;;
;;;; MAKE-STYLESHEET rules            list of CSS-NODE / NIL (dropped); else TYPE-ERROR.
;;;; MERGE-STYLESHEETS &rest sheets   a new stylesheet with the rules appended.
;;;; MAKE-MEDIA-RULE query rules      QUERY: prelude text rules (as values)
;;;;                                  -> CSS-INVALID-PRELUDE.
;;;; MAKE-SUPPORTS-RULE condition rules  same.
;;;; MAKE-LAYER-RULE names &optional (rules nil rules-p)
;;;;   NAMES: a string or list of strings, each dotted ("base.reset"); every
;;;;   segment non-empty, else CSS-INVALID-IDENTIFIER. No RULES argument ->
;;;;   statement (@layer a, b;), at least one name (none ->
;;;;   CSS-INVALID-IDENTIFIER). With RULES -> block, zero
;;;;   names (anonymous) or one; two or more names with rules ->
;;;;   CSS-INVALID-PRELUDE.
;;;; MAKE-FONT-FACE-RULE declarations
;;;; MAKE-KEYFRAMES-RULE name keyframes  NAME: non-empty string, else
;;;;                                  CSS-INVALID-IDENTIFIER (escaped on output).
;;;; MAKE-KEYFRAME selectors declarations
;;;;   SELECTORS: a string (comma list) or list of strings; each trimmed;
;;;;   "from" / "to" (any case, stored lowercased) or digits[.digits]"%";
;;;;   else CSS-INVALID-SELECTOR.
;;;; MAKE-IMPORT-RULE url &key layer supports media
;;;;   URL: a string (serialized with CSS-URL). LAYER: NIL, T (anonymous) or a
;;;;   dotted layer name. SUPPORTS / MEDIA: prelude text or NIL.
;;;; MAKE-SCOPE-RULE start rules &key end
;;;;   START / END: selector arguments as for MAKE-RULE (END may be NIL).
;;;;
;;;; Every constructor that validates text offers USE-VALUE and SKIP-INVALID
;;;; (see src/conditions.lisp). Type errors are plain CL TYPE-ERRORs.

(in-package #:css-protocol)

(defclass css-node () ()
  (:documentation "Base class of every stylesheet node."))

(defclass declaration-block ()
  ((declarations :initarg :declarations :reader rule-declarations :initform nil))
  (:documentation "Mixin: a node that holds CSS-DECLARATIONs."))

(defclass stylesheet (css-node)
  ((rules :initarg :rules :reader stylesheet-rules :initform nil)))

(defclass rule (css-node declaration-block)
  ((selectors :initarg :selectors :reader rule-selectors :initform nil))
  (:documentation "A style rule: a selector list and declarations."))

(defclass css-declaration (css-node)
  ((property :initarg :property :reader declaration-property)
   (value :initarg :value :reader declaration-value)
   (important :initarg :important :reader declaration-important-p :initform nil)))

(defclass at-rule (css-node) ()
  (:documentation "Base class of at-rules."))

(defgeneric at-rule-name (at-rule)
  (:documentation "The at-keyword without @, e.g. \"media\"."))

(defgeneric at-rule-rules (at-rule)
  (:documentation "Child rules of a grouping at-rule; NIL for others.")
  (:method ((r at-rule)) nil))

(defclass grouping-rule (at-rule)
  ((rules :initarg :rules :reader at-rule-rules :initform nil))
  (:documentation "Mixin for at-rules whose block holds rules."))

(defclass media-rule (grouping-rule)
  ((query :initarg :query :reader media-rule-query)))

(defclass supports-rule (grouping-rule)
  ((supports-condition :initarg :condition :reader supports-rule-condition)))

(defclass layer-rule (grouping-rule)
  ((names :initarg :names :reader layer-rule-names :initform nil)
   (statement-p :initarg :statement-p :reader layer-rule-statement-p :initform nil)))

(defclass font-face-rule (at-rule declaration-block) ())

(defclass keyframes-rule (at-rule)
  ((name :initarg :name :reader keyframes-rule-name)
   (keyframes :initarg :keyframes :reader keyframes-rule-keyframes :initform nil)))

(defclass keyframe (css-node declaration-block)
  ((selectors :initarg :selectors :reader keyframe-selectors :initform nil)))

(defclass import-rule (at-rule)
  ((url :initarg :url :reader import-rule-url)
   (layer :initarg :layer :reader import-rule-layer :initform nil)
   (supports :initarg :supports :reader import-rule-supports :initform nil)
   (media :initarg :media :reader import-rule-media :initform nil)))

(defclass scope-rule (grouping-rule)
  ((start :initarg :start :reader scope-rule-start :initform nil)
   (end :initarg :end :reader scope-rule-end :initform nil)))

(defmethod at-rule-name ((r media-rule)) "media")
(defmethod at-rule-name ((r supports-rule)) "supports")
(defmethod at-rule-name ((r layer-rule)) "layer")
(defmethod at-rule-name ((r font-face-rule)) "font-face")
(defmethod at-rule-name ((r keyframes-rule)) "keyframes")
(defmethod at-rule-name ((r import-rule)) "import")
(defmethod at-rule-name ((r scope-rule)) "scope")

;;; Character classes (ASCII only where the grammar says so: DIGIT-CHAR-P and
;;; ALPHANUMERICP accept non-ASCII characters in SBCL).

(defun %css-whitespace-p (c)
  (member c '(#\Space #\Tab #\Return #\Newline #\Page)))

(defun %newline-p (c)
  (member c '(#\Newline #\Return #\Page)))

(defun %ascii-alpha-p (c)
  (or (char<= #\a c #\z) (char<= #\A c #\Z)))

(defun %ascii-digit-p (c)
  (char<= #\0 c #\9))

(defun %hex-digit-p (c)
  (or (%ascii-digit-p c) (char<= #\a c #\f) (char<= #\A c #\F)))

(defun %ascii-name-char-p (c)
  "[A-Za-z0-9_-]"
  (or (%ascii-alpha-p c) (%ascii-digit-p c) (char= c #\_) (char= c #\-)))

(defun %name-char-p (c)
  "[A-Za-z0-9_-] or a character >= U+0080."
  (or (%ascii-name-char-p c) (>= (char-code c) #x80)))

(defparameter +css-whitespace+
  (coerce '(#\Space #\Tab #\Return #\Newline #\Page) 'string))

;;; Text scanner shared by values, preludes and selectors.

(defun %scan-string-end (text i)
  "TEXT[I] is a quote. The index after the closing quote, or NIL when the
string is unterminated or holds a raw newline. A backslash escapes the next
character (a backslash + newline is a line continuation)."
  (let ((quote (char text i))
        (n (length text))
        (j (1+ i)))
    (loop
      (when (>= j n) (return nil))
      (let ((c (char text j)))
        (cond ((char= c #\\) (incf j 2))
              ((char= c quote) (return (1+ j)))
              ((%newline-p c) (return nil))
              (t (incf j)))))))

(defun %escape-end (text i)
  "TEXT[I] is a backslash outside a string. The index after the escape (1-6
hex digits plus one optional whitespace, CRLF counting as one; or any one
non-newline character), or NIL for a trailing backslash or backslash +
newline."
  (let ((n (length text))
        (j (1+ i)))
    (cond ((>= j n) nil)
          ((%newline-p (char text j)) nil)
          ((%hex-digit-p (char text j))
           (let* ((limit (min n (+ j 6)))
                  (k (or (position-if-not #'%hex-digit-p text :start j :end limit)
                         limit)))
             (cond ((and (< (1+ k) n)
                         (char= (char text k) #\Return)
                         (char= (char text (1+ k)) #\Newline))
                    (+ k 2))
                   ((and (< k n) (%css-whitespace-p (char text k))) (1+ k))
                   (t k))))
          (t (1+ j)))))

(defun %escape-char (text i end)
  "Decode the escape TEXT[I,END) found by %ESCAPE-END."
  (let ((j (1+ i)))
    (if (%hex-digit-p (char text j))
        (let* ((k (or (position-if-not #'%hex-digit-p text :start j :end end) end))
               (code (parse-integer text :start j :end k :radix 16)))
          (if (or (zerop code) (<= #xD800 code #xDFFF) (> code #x10FFFF))
              (code-char #xFFFD)
              (code-char code)))
        (char text j))))

(defun %normalise-text (text mode class)
  "Validate and normalise value (MODE :value) or prelude (MODE :prelude)
TEXT. Comments outside strings become one space, whitespace runs outside
strings collapse to one space, the result is trimmed. Refuses with CLASS."
  (declare (type string text))
  (flet ((refuse (why) (%refuse class why)))
    (when (search "</" text) (refuse "contains </"))
    (let ((n (length text))
          (i 0)
          (stack '())
          (space nil)
          (any nil))
      (let ((out
              (with-output-to-string (o)
                (flet ((emit (start end)
                         (when (and space any) (write-char #\Space o))
                         (setf space nil any t)
                         (write-string text o :start start :end end)))
                  (loop while (< i n)
                        do (let ((c (char text i)))
                             (cond
                               ((%css-whitespace-p c) (setf space t) (incf i))
                               ((and (char= c #\/) (< (1+ i) n)
                                     (char= (char text (1+ i)) #\*))
                                (let ((end (search "*/" text :start2 (+ i 2))))
                                  (unless end (refuse "unterminated comment"))
                                  (setf space t i (+ end 2))))
                               ((or (char= c #\") (char= c #\'))
                                (let ((end (%scan-string-end text i)))
                                  (unless end
                                    (refuse "unterminated string or raw newline in a string"))
                                  (emit i end)
                                  (setf i end)))
                               ((char= c #\\)
                                (let ((end (%escape-end text i)))
                                  (unless end (refuse "unterminated escape"))
                                  (emit i end)
                                  (setf i end)))
                               ((find c "([{")
                                (when (and (eq mode :prelude) (char= c #\{))
                                  (refuse "{ in an at-rule prelude"))
                                (push (ecase c (#\( #\)) (#\[ #\]) (#\{ #\})) stack)
                                (emit i (1+ i))
                                (incf i))
                               ((find c ")]}")
                                (unless (eql c (pop stack))
                                  (refuse "unbalanced brackets"))
                                (emit i (1+ i))
                                (incf i))
                               ((and (find c ";!") (null stack))
                                (refuse (format nil "~C outside strings and brackets" c)))
                               (t (emit i (1+ i)) (incf i)))))))))
        (when stack (refuse "unclosed bracket"))
        (unless any (refuse "empty"))
        out))))

;;; Numbers

(defun %format-real (x)
  "Plain decimal text for a real: no exponent, no trailing zeros or dot."
  (etypecase x
    (integer (format nil "~D" x))
    (ratio (%format-real (float x 1d0)))
    (float
     (when (or (/= x x) (> (abs x) most-positive-double-float))
       (%refuse 'css-invalid-value "not a finite number"))
     (let* ((text (format nil "~F" x))
            (text (if (find #\. text)
                      (string-right-trim "." (string-right-trim "0" text))
                      text)))
       (if (string= text "-0") "0" text)))))

;;; Validators (signal through %REFUSE; driven by %VALIDATE)

(defun %property-text (property)
  (%require-type property 'string)
  (let ((n (length property)))
    (cond ((and (> n 2)
                (string= "--" property :end2 2)
                (every #'%name-char-p (subseq property 2)))
           (copy-seq property))
          ((let ((s (if (and (plusp n) (char= (char property 0) #\-)) 1 0)))
             (and (< s n)
                  (let ((c (char property s)))
                    (or (%ascii-alpha-p c) (char= c #\_)))
                  (every #'%ascii-name-char-p (subseq property s))))
           (string-downcase property))
          (t (%refuse 'css-invalid-property
                      "not an identifier or a --custom property name")))))

(defun %value-text (value)
  (%require-type value '(or string real))
  (if (realp value)
      (%format-real value)
      (%normalise-text value :value 'css-invalid-value)))

(defun %prelude-text (prelude)
  (%require-type prelude 'string)
  (%normalise-text prelude :prelude 'css-invalid-prelude))

(defun %identifier-text (name)
  (%require-type name 'string)
  (if (plusp (length name))
      (copy-seq name)
      (%refuse 'css-invalid-identifier "empty identifier")))

(defun %layer-name-text (name)
  (%require-type name 'string)
  (if (and (plusp (length name))
           (loop for start = 0 then (1+ dot)
                 for dot = (position #\. name :start start)
                 always (< start (or dot (length name)))
                 while dot))
      (copy-seq name)
      (%refuse 'css-invalid-identifier "empty layer name or name segment")))

(defun %layer-names (names statement-p)
  (let ((names (mapcar #'%layer-name-text
                       (%require-type (if (stringp names) (list names) names) 'list))))
    (cond ((and statement-p (null names))
           (%refuse 'css-invalid-identifier "a layer statement needs a name"))
          ((and (not statement-p) (rest names))
           (%refuse 'css-invalid-prelude "a layer block takes at most one name"))
          (t names))))

(defun %percentage-p (s)
  "digits[.digits]%"
  (let* ((n (length s))
         (end (1- n)))
    (and (> n 1)
         (char= (char s end) #\%)
         (let ((dot (position #\. s :end end)))
           (if dot
               (and (plusp dot)
                    (< (1+ dot) end)
                    (every #'%ascii-digit-p (subseq s 0 dot))
                    (every #'%ascii-digit-p (subseq s (1+ dot) end)))
               (every #'%ascii-digit-p (subseq s 0 end)))))))

(defun %split-commas (string)
  (loop for start = 0 then (1+ comma)
        for comma = (position #\, string :start start)
        collect (subseq string start comma)
        while comma))

(defun %keyframe-selectors (selectors)
  (let ((items (if (stringp selectors) (list selectors) (%require-type selectors 'list))))
    (or (loop for item in items
              nconc (loop for part in (%split-commas (%require-type item 'string))
                          for text = (string-trim +css-whitespace+ part)
                          collect (cond ((string-equal text "from") "from")
                                        ((string-equal text "to") "to")
                                        ((%percentage-p text) text)
                                        (t (%refuse 'css-invalid-selector
                                                    "not from, to or a percentage")))))
        (%refuse 'css-invalid-selector "no keyframe selector"))))

;;; Child lists and selector arguments

(defun %children (list type)
  "A fresh list of LIST's non-NIL elements, each of TYPE (else TYPE-ERROR)."
  (loop for x in (%require-type list 'list)
        when x collect (%require-type x type)))

(deftype %rule-child () '(or rule at-rule))

(defun %selector-argument (argument)
  "A string (comma list), a SELECTOR, or a non-empty list of those -> (values
SELECTORS T), or (values NIL NIL) when a selector string was skipped."
  (let ((items (if (listp argument) argument (list argument)))
        (out '()))
    (when (null items)
      (error 'type-error :datum argument :expected-type '(or string selector cons)))
    (dolist (x items (values (nreverse out) t))
      (typecase x
        (selector (push x out))
        (string (let ((list (parse-selector-list x)))
                  (unless list (return (values nil nil)))
                  (dolist (s list) (push s out))))
        (t (error 'type-error :datum x :expected-type '(or string selector)))))))

;;; Constructors

(defun custom-property-p (declaration-or-property)
  "True when the property (a CSS-DECLARATION's or a string) starts with --."
  (let ((p (if (typep declaration-or-property 'css-declaration)
               (declaration-property declaration-or-property)
               declaration-or-property)))
    (and (stringp p) (>= (length p) 2) (string= "--" p :end2 2) t)))

(defun make-declaration (property value &key important)
  (%require-type property 'string)
  (%require-type value '(or string real))
  (multiple-value-bind (property ok) (%validate property #'%property-text)
    (when ok
      (multiple-value-bind (value ok) (%validate value #'%value-text)
        (when ok
          (make-instance 'css-declaration :property property :value value
                                          :important (and important t)))))))

(defun make-rule (selectors declarations)
  (multiple-value-bind (selectors ok) (%selector-argument selectors)
    (when ok
      (make-instance 'rule :selectors selectors
                           :declarations (%children declarations 'css-declaration)))))

(defun make-stylesheet (rules)
  (make-instance 'stylesheet :rules (%children rules '%rule-child)))

(defun merge-stylesheets (&rest stylesheets)
  (make-instance 'stylesheet
                 :rules (loop for s in stylesheets
                              append (copy-list (stylesheet-rules (%require-type s 'stylesheet))))))

(defun make-media-rule (query rules)
  (%require-type query 'string)
  (multiple-value-bind (query ok) (%validate query #'%prelude-text)
    (when ok
      (make-instance 'media-rule :query query :rules (%children rules '%rule-child)))))

(defun make-supports-rule (condition rules)
  (%require-type condition 'string)
  (multiple-value-bind (condition ok) (%validate condition #'%prelude-text)
    (when ok
      (make-instance 'supports-rule :condition condition
                                    :rules (%children rules '%rule-child)))))

(defun make-layer-rule (names &optional (rules nil rules-p))
  (let ((statement-p (not rules-p)))
    (multiple-value-bind (names ok)
        (%validate names (lambda (names) (%layer-names names statement-p)))
      (when ok
        (make-instance 'layer-rule :names names :statement-p statement-p
                                   :rules (%children rules '%rule-child))))))

(defun make-font-face-rule (declarations)
  (make-instance 'font-face-rule
                 :declarations (%children declarations 'css-declaration)))

(defun make-keyframes-rule (name keyframes)
  (%require-type name 'string)
  (multiple-value-bind (name ok) (%validate name #'%identifier-text)
    (when ok
      (make-instance 'keyframes-rule :name name
                                     :keyframes (%children keyframes 'keyframe)))))

(defun make-keyframe (selectors declarations)
  (multiple-value-bind (selectors ok) (%validate selectors #'%keyframe-selectors)
    (when ok
      (make-instance 'keyframe :selectors selectors
                               :declarations (%children declarations 'css-declaration)))))

(defun make-import-rule (url &key layer supports media)
  (%require-type url 'string)
  (%require-type layer '(or boolean string))
  (%require-type supports '(or null string))
  (%require-type media '(or null string))
  (block nil
    (flet ((check (value validator)
             (if (or (null value) (eq value t))
                 value
                 (multiple-value-bind (text ok) (%validate value validator)
                   (unless ok (return nil))
                   text))))
      (let* ((layer (check layer #'%layer-name-text))
             (supports (check supports #'%prelude-text))
             (media (check media #'%prelude-text)))
        (make-instance 'import-rule :url (copy-seq url) :layer layer
                                    :supports supports :media media)))))

(defun make-scope-rule (start rules &key end)
  (multiple-value-bind (start ok) (%selector-argument start)
    (when ok
      (multiple-value-bind (end ok) (if end (%selector-argument end) (values nil t))
        (when ok
          (make-instance 'scope-rule :start start :end end
                                     :rules (%children rules '%rule-child)))))))

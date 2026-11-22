;;;; Selectors. Classes and readers GIVEN; parsing and text are STUBS.
;;;;
;;;; PARSE-SELECTOR-LIST string -> list of SELECTOR, one per top-level comma.
;;;; Grammar (0.1.0 subset; brief "Selector subset"):
;;;;
;;;;   list      := complex ( "," complex )*        each complex non-empty
;;;;   complex   := compound ( combinator compound )*
;;;;   combinator:= whitespace (:descendant) | ">" (:child) | "+" (:next-sibling)
;;;;              | "~" (:subsequent-sibling), optional whitespace around
;;;;   compound  := [ type | "*" ] simple*            at least one part
;;;;   simple    := "#" ident | "." ident | "[" ... "]"
;;;;              | ":" ident [ "(" ... ")" ] | "::" ident [ "(" ... ")" ]
;;;;   ident     := CSS identifier with escapes ("\" 1-6 hex digits + one
;;;;                optional whitespace, or "\" + any non-newline char).
;;;;                Starts with a letter, "_", a char >= U+0080, an escape, or
;;;;                "-" followed by one of those or "-".
;;;;
;;;; Bracket / paren contents must be non-empty for [], balanced, with
;;;; strings closed; they are kept verbatim with whitespace runs collapsed.
;;;; Refused with CSS-INVALID-SELECTOR (input = the whole string): empty or
;;;; blank text, empty list members, leading / trailing / doubled combinators,
;;;; "|" (namespaces), "&", "{", "}", ";", "<" outside strings, "</"
;;;; anywhere, unterminated strings / brackets, a class or id without a valid
;;;; identifier (".1x", "#", ".", "a..b"). A non-string is a TYPE-ERROR.
;;;; Restarts: USE-VALUE (replacement text, parsed again), SKIP-INVALID (NIL).
;;;;
;;;; SELECTOR-COMPOUNDS: (compound combinator compound ...), combinators as the
;;;; keywords above. COMPOUND-TYPE: lowercased type, "*" or NIL.
;;;; COMPOUND-IDS / COMPOUND-CLASSES: unescaped names in source order.
;;;; COMPOUND-OTHERS: verbatim attribute / pseudo parts in source order
;;;; (":hover", "::before", ":not(.x)", "[type=\"text\"]").
;;;;
;;;; SELECTOR-TEXT selector &key (pretty t): type, then ids / classes / others
;;;; in their source order (ids and classes re-escaped with CSS-IDENT).
;;;; Combinators " > " " + " " ~ " " " pretty; ">" "+" "~" " " minified.

(in-package #:css-protocol)

(defclass compound-selector ()
  ((type :initarg :type :reader compound-type :initform nil)
   (ids :initarg :ids :reader compound-ids :initform nil)
   (classes :initarg :classes :reader compound-classes :initform nil)
   (others :initarg :others :reader compound-others :initform nil)
   (parts :initarg :parts :reader compound-parts :initform nil
          :documentation "Source order of the non-type parts: (:id name),
(:class name) or (:other text). Used by SELECTOR-TEXT."))
  (:documentation "One compound selector: div.a#b:hover."))

(defclass selector ()
  ((compounds :initarg :compounds :reader selector-compounds :initform nil))
  (:documentation "One complex selector of a selector list."))

(defun %make-compound (type parts)
  "A compound whose ids / classes / others are derived from PARTS."
  (make-instance 'compound-selector
                 :type type
                 :parts parts
                 :ids (loop for (kind value) in parts when (eq kind :id) collect value)
                 :classes (loop for (kind value) in parts when (eq kind :class) collect value)
                 :others (loop for (kind value) in parts when (eq kind :other) collect value)))

(defun %collapse-whitespace (text start end)
  "TEXT[START,END) with whitespace runs outside strings collapsed to one space."
  (with-output-to-string (o)
    (let ((i start)
          (space nil))
      (loop while (< i end)
            do (let ((c (char text i)))
                 (cond ((%css-whitespace-p c) (setf space t) (incf i))
                       (t
                        (when space (write-char #\Space o) (setf space nil))
                        (let ((next (cond ((or (char= c #\") (char= c #\'))
                                           (%scan-string-end text i))
                                          ((char= c #\\) (%escape-end text i))
                                          (t (1+ i)))))
                          (write-string text o :start i :end next)
                          (setf i next))))))
      (when space (write-char #\Space o)))))

(defun %parse-selectors (text)
  "Parse TEXT as a selector list (the 0.1.0 subset). Refuses through %REFUSE."
  (%require-type text 'string)
  (let ((n (length text))
        (pos 0))
    (labels ((fail (why)
               (%refuse 'css-invalid-selector why))
             (peek (&optional (offset 0))
               (let ((i (+ pos offset)))
                 (and (< i n) (char text i))))
             (skip-whitespace ()
               (let ((start pos))
                 (loop while (and (< pos n) (%css-whitespace-p (char text pos)))
                       do (incf pos))
                 (> pos start)))
             (escape-at-p (i)
               (and (< (1+ i) n)
                    (char= (char text i) #\\)
                    (not (%newline-p (char text (1+ i))))))
             (ident-start-p (i)
               (and (< i n)
                    (let ((c (char text i)))
                      (cond ((or (%ascii-alpha-p c) (char= c #\_) (>= (char-code c) #x80)) t)
                            ((char= c #\\) (escape-at-p i))
                            ((char= c #\-)
                             (let ((j (1+ i)))
                               (and (< j n)
                                    (let ((d (char text j)))
                                      (or (%ascii-alpha-p d) (char= d #\_) (char= d #\-)
                                          (>= (char-code d) #x80)
                                          (escape-at-p j))))))
                            (t nil)))))
             (ident ()
               (unless (ident-start-p pos) (fail "expected an identifier"))
               (with-output-to-string (o)
                 (loop while (< pos n)
                       do (let ((c (char text pos)))
                            (cond ((char= c #\\)
                                   (let ((end (%escape-end text pos)))
                                     (unless end (fail "unterminated escape"))
                                     (write-char (%escape-char text pos end) o)
                                     (setf pos end)))
                                  ((%name-char-p c) (write-char c o) (incf pos))
                                  (t (return)))))))
             (group ()
               ;; POS is at an opening ( or [; move past its matching close.
               (let ((stack '()))
                 (loop
                   (when (>= pos n) (fail "unterminated bracket"))
                   (let ((c (char text pos)))
                     (cond ((or (char= c #\") (char= c #\'))
                            (let ((end (%scan-string-end text pos)))
                              (unless end (fail "unterminated string"))
                              (setf pos end)))
                           ((char= c #\\)
                            (let ((end (%escape-end text pos)))
                              (unless end (fail "unterminated escape"))
                              (setf pos end)))
                           ((find c "([")
                            (push (if (char= c #\() #\) #\]) stack)
                            (incf pos))
                           ((find c ")]")
                            (unless (eql c (pop stack)) (fail "mismatched bracket"))
                            (incf pos)
                            (when (null stack) (return)))
                           ((find c "{};&<")
                            (fail (format nil "~C is not allowed in a selector" c)))
                           ((and (char= c #\|) (not (eql (peek 1) #\=)))
                            (fail "namespaces are not supported"))
                           (t (incf pos)))))))
             (compound ()
               (let ((type nil)
                     (parts '()))
                 (cond ((eql (peek) #\*) (incf pos) (setf type "*"))
                       ((ident-start-p pos) (setf type (string-downcase (ident)))))
                 (loop
                   (case (peek)
                     (#\# (incf pos) (push (list :id (ident)) parts))
                     (#\. (incf pos) (push (list :class (ident)) parts))
                     (#\[ (let ((start pos))
                            (group)
                            (when (zerop (length (string-trim +css-whitespace+
                                                              (subseq text (1+ start) (1- pos)))))
                              (fail "empty attribute selector"))
                            (push (list :other (%collapse-whitespace text start pos)) parts)))
                     (#\: (let ((start pos))
                            (incf pos)
                            (when (eql (peek) #\:) (incf pos))
                            (ident)
                            (when (eql (peek) #\() (group))
                            (push (list :other (%collapse-whitespace text start pos)) parts)))
                     (t (return))))
                 (when (or type parts)
                   (%make-compound type (nreverse parts)))))
             (complex-selector ()
               (let ((items (list (or (compound) (fail "expected a compound selector")))))
                 (loop
                   (let* ((whitespace (skip-whitespace))
                          (c (peek)))
                     (cond ((or (null c) (char= c #\,)) (return))
                           ((find c ">+~")
                            (incf pos)
                            (skip-whitespace)
                            (push (ecase c
                                    (#\> :child)
                                    (#\+ :next-sibling)
                                    (#\~ :subsequent-sibling))
                                  items)
                            (push (or (compound) (fail "a combinator needs a compound after it"))
                                  items))
                           (whitespace
                            (push :descendant items)
                            (push (or (compound) (fail (format nil "unexpected ~C" c))) items))
                           (t (fail (format nil "unexpected ~C" c))))))
                 (make-instance 'selector :compounds (nreverse items)))))
      (when (search "</" text) (fail "contains </"))
      (loop collect (progn (skip-whitespace) (complex-selector))
            until (>= pos n)
            do (unless (char= (char text pos) #\,) (fail "expected ,"))
               (incf pos)))))

(defun parse-selector-list (string)
  "STRING -> list of SELECTOR, one per top-level comma. Signals
CSS-INVALID-SELECTOR (input = STRING) with USE-VALUE and SKIP-INVALID."
  (%require-type string 'string)
  (values (%validate string #'%parse-selectors)))

(defun selector-text (selector &key (pretty t))
  "Canonical text of SELECTOR: type, then parts in source order."
  (%require-type selector 'selector)
  (with-output-to-string (o)
    (dolist (item (selector-compounds selector))
      (if (keywordp item)
          (write-string (ecase item
                          (:descendant " ")
                          (:child (if pretty " > " ">"))
                          (:next-sibling (if pretty " + " "+"))
                          (:subsequent-sibling (if pretty " ~ " "~")))
                        o)
          (let ((type (compound-type item)))
            (when type
              (write-string (if (string= type "*") "*" (css-ident type)) o))
            (loop for (kind value) in (compound-parts item)
                  do (ecase kind
                       (:id (write-char #\# o) (write-string (css-ident value) o))
                       (:class (write-char #\. o) (write-string (css-ident value) o))
                       (:other (write-string value o)))))))))

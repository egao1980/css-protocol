;;;; Value helpers. STUBS. Brief "Escaping".
;;;;
;;;; CSS-IDENT string -> CSSOM "serialize an identifier":
;;;;   U+0000 -> U+FFFD (the character itself);
;;;;   U+0001-U+001F, U+007F -> "\" + lowercase hex + " ";
;;;;   a digit at index 0, or at index 1 when index 0 is "-" -> "\" + hex + " ";
;;;;   the string "-" alone -> "\-";
;;;;   "-", "_", [0-9A-Za-z], chars >= U+0080 -> as is;
;;;;   any other char -> "\" + the char.
;;;;   "" -> CSS-INVALID-IDENTIFIER (USE-VALUE: escape the replacement instead;
;;;;   SKIP-INVALID: return NIL). Non-string -> TYPE-ERROR.
;;;;
;;;; CSS-STRING string -> double-quoted CSSOM "serialize a string":
;;;;   U+0000 -> U+FFFD; U+0001-U+001F, U+007F -> "\" + hex + " ";
;;;;   "\"" and "\\" -> backslash + char; "<" -> "\3c "; others as is.
;;;;
;;;; CSS-URL string -> "url(" + (css-string string) + ")".
;;;;
;;;; CSS-VAR name &optional fallback -> "var(--NAME)" or "var(--NAME, FALLBACK)".
;;;;   A leading "--" in NAME is accepted; the rest goes through CSS-IDENT.
;;;;   FALLBACK is a string or real, normalised like a declaration value.

(in-package #:css-protocol)

(defun %write-hex-escape (code stream)
  (format stream "\\~(~X~) " code))

(defun %serialize-ident (string)
  "CSSOM serialize-an-identifier of a non-empty STRING."
  (if (string= string "-")
      "\\-"
      (with-output-to-string (o)
        (loop for c across string
              for i from 0
              for code = (char-code c)
              do (cond ((zerop code) (write-char (code-char #xFFFD) o))
                       ((or (<= 1 code #x1F) (= code #x7F)) (%write-hex-escape code o))
                       ((and (%ascii-digit-p c)
                             (or (= i 0)
                                 (and (= i 1) (char= (char string 0) #\-))))
                        (%write-hex-escape code o))
                       ((%name-char-p c) (write-char c o))
                       (t (write-char #\\ o) (write-char c o)))))))

(defun %ident-text (string)
  (if (zerop (length (%require-type string 'string)))
      (%refuse 'css-invalid-identifier "empty identifier")
      (%serialize-ident string)))

(defun css-ident (string)
  "STRING serialized as a CSS identifier. Signals CSS-INVALID-IDENTIFIER for
\"\" with USE-VALUE (escape the replacement) and SKIP-INVALID (NIL)."
  (%require-type string 'string)
  (values (%validate string #'%ident-text)))

(defun css-string (string)
  "STRING as a double-quoted CSS string; < is escaped as \\3c ."
  (%require-type string 'string)
  (with-output-to-string (o)
    (write-char #\" o)
    (loop for c across string
          for code = (char-code c)
          do (cond ((zerop code) (write-char (code-char #xFFFD) o))
                   ((or (<= 1 code #x1F) (= code #x7F)) (%write-hex-escape code o))
                   ((or (char= c #\") (char= c #\\)) (write-char #\\ o) (write-char c o))
                   ((char= c #\<) (write-string "\\3c " o))
                   (t (write-char c o))))
    (write-char #\" o)))

(defun css-url (string)
  "url(\"...\") with STRING through CSS-STRING."
  (concatenate 'string "url(" (css-string string) ")"))

(defun css-var (name &optional fallback)
  "var(--NAME) or var(--NAME, FALLBACK). FALLBACK is normalised like a
declaration value (CSS-INVALID-VALUE with USE-VALUE / SKIP-INVALID)."
  (%require-type name 'string)
  (%require-type fallback '(or null string real))
  (let* ((bare (if (and (>= (length name) 2) (string= "--" name :end2 2))
                   (subseq name 2)
                   name))
         (ident (css-ident bare)))
    (when ident
      (if (null fallback)
          (format nil "var(--~A)" ident)
          (multiple-value-bind (text ok) (%validate fallback #'%value-text)
            (when ok (format nil "var(--~A, ~A)" ident text)))))))

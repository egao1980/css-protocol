;;;; Custom-property themes. Class GIVEN; constructor and stylesheet STUBS.
;;;; Brief "Themes".
;;;;
;;;; MAKE-CSS-THEME &key light dark (selector ":root") toggle-attribute
;;;;   LIGHT / DARK: alists or plists of key -> value (a list whose first
;;;;   element is a cons is an alist, else a plist). Keys: strings with or
;;;;   without the "--" prefix, or symbols (name downcased). Stored normalised
;;;;   as alists of ("--name" . value-text) in the given order, values
;;;;   normalised as declaration values (MAKE-DECLARATION rules + restarts).
;;;;   SELECTOR: replaces ":root" in every rule below.
;;;;   TOGGLE-ATTRIBUTE: NIL or an attribute name such as "data-theme".
;;;;
;;;; CSS-THEME-STYLESHEET theme -> STYLESHEET with, in order:
;;;;   1. SEL { [color-scheme: light dark;  -- only when DARK is non-empty]
;;;;            light custom properties }
;;;;   2. when DARK: @media (prefers-color-scheme: dark) {
;;;;        SEL { dark custom properties } }   where SEL is
;;;;        SEL:not([ATTR="light"]) when TOGGLE-ATTRIBUTE is set;
;;;;   3. when DARK and TOGGLE-ATTRIBUTE:
;;;;        SEL[ATTR="dark"] { color-scheme: dark; dark custom properties }
;;;;        SEL[ATTR="light"] { color-scheme: light; }

(in-package #:css-protocol)

(defclass css-theme ()
  ((light :initarg :light :reader css-theme-light :initform nil)
   (dark :initarg :dark :reader css-theme-dark :initform nil)
   (selector :initarg :selector :reader css-theme-selector :initform ":root")
   (toggle-attribute :initarg :toggle-attribute :reader css-theme-toggle-attribute
                     :initform nil)))

(defun %theme-property (key)
  "\"--name\" for a string key (with or without --) or a symbol (downcased)."
  (let ((name (typecase key
                (string key)
                (symbol (string-downcase (symbol-name key)))
                (t (error 'type-error :datum key :expected-type '(or string symbol))))))
    (if (custom-property-p name) name (concatenate 'string "--" name))))

(defun %theme-map (map)
  "Alist or plist MAP -> alist of (\"--name\" . value-text), validated through
MAKE-DECLARATION (its conditions and restarts; skipped entries are dropped)."
  (let ((pairs (if (consp (first (%require-type map 'list)))
                   (loop for entry in map
                         collect (cons (car (%require-type entry 'cons)) (cdr entry)))
                   (loop for (key value) on map by #'cddr
                         collect (cons key value)))))
    (loop for (key . value) in pairs
          for declaration = (make-declaration (%theme-property key) value)
          when declaration
            collect (cons (declaration-property declaration)
                          (declaration-value declaration)))))

(defun make-css-theme (&key light dark (selector ":root") toggle-attribute)
  (%require-type selector 'string)
  (%require-type toggle-attribute '(or null string))
  (make-instance 'css-theme :light (%theme-map light)
                            :dark (%theme-map dark)
                            :selector selector
                            :toggle-attribute toggle-attribute))

(defun css-theme-stylesheet (theme)
  (%require-type theme 'css-theme)
  (let ((selector (css-theme-selector theme))
        (attribute (css-theme-toggle-attribute theme))
        (light (css-theme-light theme))
        (dark (css-theme-dark theme)))
    (labels ((declarations (alist)
               (loop for (property . value) in alist
                     collect (make-declaration property value)))
             (scheme (value)
               (make-declaration "color-scheme" value))
             (attribute-test (value)
               (format nil "[~A=~A]" (css-ident attribute) (css-string value)))
             (suffixed (suffix)
               ;; Every complex selector of SELECTOR with SUFFIX on its last compound.
               (mapcar (lambda (s) (concatenate 'string (selector-text s :pretty nil) suffix))
                       (parse-selector-list selector))))
      (make-stylesheet
       (list* (make-rule selector
                         (append (when dark (list (scheme "light dark")))
                                 (declarations light)))
              (when dark
                (list* (make-media-rule
                        "(prefers-color-scheme: dark)"
                        (list (make-rule (if attribute
                                             (suffixed (format nil ":not(~A)"
                                                               (attribute-test "light")))
                                             selector)
                                         (declarations dark))))
                       (when attribute
                         (list (make-rule (suffixed (attribute-test "dark"))
                                          (cons (scheme "dark") (declarations dark)))
                               (make-rule (suffixed (attribute-test "light"))
                                          (list (scheme "light"))))))))))))

;;;; css-protocol: a CLOS stylesheet model, serializer, themes and transforms.
;;;; Brief: cl-stack/docs/capabilities/css.md (H4h-0b-0).

(defpackage #:css-protocol
  (:use #:cl)
  (:nicknames #:stack-css)
  (:export
   ;; conditions + restarts (src/conditions.lisp)
   #:css-error
   #:css-error-message
   #:css-syntax-error
   #:css-error-input
   #:css-invalid-selector
   #:css-invalid-property
   #:css-invalid-value
   #:css-invalid-identifier
   #:css-invalid-prelude
   #:css-no-backend
   #:css-no-backend-source
   #:skip-invalid
   #:use-backend

   ;; backends (src/protocol.lisp)
   #:*css-backend*
   #:css-backend
   #:backend-compile-stylesheet
   #:compile-stylesheet

   ;; serializer (src/serialize.lisp)
   #:write-stylesheet
   #:write-css-node

   ;; values (src/values.lisp)
   #:css-ident
   #:css-string
   #:css-url
   #:css-var

   ;; selectors (src/selector.lisp)
   #:selector
   #:selector-compounds
   #:selector-text
   #:parse-selector-list
   #:compound-selector
   #:compound-type
   #:compound-ids
   #:compound-classes
   #:compound-others

   ;; model (src/model.lisp)
   #:css-node
   #:stylesheet
   #:stylesheet-rules
   #:make-stylesheet
   #:merge-stylesheets
   #:rule
   #:rule-selectors
   #:rule-declarations
   #:make-rule
   #:css-declaration
   #:declaration-property
   #:declaration-value
   #:declaration-important-p
   #:custom-property-p
   #:make-declaration
   #:at-rule
   #:at-rule-name
   #:at-rule-rules
   #:media-rule
   #:media-rule-query
   #:make-media-rule
   #:supports-rule
   #:supports-rule-condition
   #:make-supports-rule
   #:layer-rule
   #:layer-rule-names
   #:layer-rule-statement-p
   #:make-layer-rule
   #:font-face-rule
   #:make-font-face-rule
   #:keyframes-rule
   #:keyframes-rule-name
   #:keyframes-rule-keyframes
   #:make-keyframes-rule
   #:keyframe
   #:keyframe-selectors
   #:make-keyframe
   #:import-rule
   #:import-rule-url
   #:import-rule-layer
   #:import-rule-supports
   #:import-rule-media
   #:make-import-rule
   #:scope-rule
   #:scope-rule-start
   #:scope-rule-end
   #:make-scope-rule

   ;; themes (src/theme.lisp)
   #:css-theme
   #:css-theme-light
   #:css-theme-dark
   #:css-theme-selector
   #:css-theme-toggle-attribute
   #:make-css-theme
   #:css-theme-stylesheet

   ;; transforms (src/transform.lisp)
   #:scope-stylesheet
   #:used-rules))

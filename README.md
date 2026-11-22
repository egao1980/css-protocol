# css-protocol

CSS stylesheets for [cl-stack](https://github.com/egao1980/cl-stack): a CLOS model (`stylesheet`, `rule`, selectors, `css-declaration`, `@media` / `@supports` / `@layer` / `@font-face` / `@keyframes` / `@import` / `@scope`), a pretty and minified serializer with CSSOM escaping (and `<` escaped, so output is safe inside `<style>`), `prefers-color-scheme` custom-property themes, `scope-stylesheet` and `used-rules` (prune a stylesheet against an [html-protocol](https://github.com/egao1980/html-protocol) tree).

**Status:** contract for slice H4h-0b-0 (0.1.0): classes, GFs and conditions are given, behaviour is stubbed (`%css0-pending`), and `css-protocol/tests` is the spec. Brief: [cl-stack/docs/capabilities/css.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/css.md). Sources go through `compile-stylesheet` backends, starting with [`css-backend-lass`](https://github.com/egao1980/css-backend-lass).

```lisp
(stack-css:write-stylesheet
  (stack-css:make-stylesheet
    (list (stack-css:make-rule "a, b > c"
            (list (stack-css:make-declaration "color" "red")
                  (stack-css:make-declaration "margin" "0 auto" :important t)))))
  :pretty nil)
;; => "a,b>c{color:red;margin:0 auto!important}"
```

## License

MIT, see [LICENSE](LICENSE).

# css-protocol

CSS stylesheets for [cl-stack](https://github.com/egao1980/cl-stack): a CLOS model (`stylesheet`, `rule`, selectors, declarations, `@media` / `@supports` / `@layer` / `@font-face` / `@keyframes` / `@import` / `@scope`), a pretty and minified serializer with correct escaping, `prefers-color-scheme` themes, `scope-stylesheet` and `used-rules` (prune a stylesheet against an [html-protocol](https://github.com/egao1980/html-protocol) tree).

**Status:** planned (slice H4h-0b-0). Brief: [cl-stack/docs/capabilities/css.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/css.md). Sources go through `compile-stylesheet` backends, starting with [`css-backend-lass`](https://github.com/egao1980/css-backend-lass).

## License

MIT, see [LICENSE](LICENSE).

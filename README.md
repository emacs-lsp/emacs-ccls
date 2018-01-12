# emacs-cquery

emacs-cquery is a client for [cquery](https://github.com/jacobdufault/cquery), a low-latency language server supporting multi-million line C++ code-bases, powered by libclang.

It leverages [lsp-mode](https://github.com/emacs-lsp/lsp-mode), but also provides some cquery extensions to LSP (e.g. `$cquery/base` `$cquery/callers` `$cquery/derived` `$cquery/vars`).

## Quickstart

```elisp
(require 'cquery)
(setq cquery-executable "/path/to/cquery/build/release/bin/cquery")
;; (setq cquery-executable "/path/to/cquery-install-prefix/bin/cquery")
```

To enable comments and use Message Pack for cache files (which are stored in `cacheDirectory`):

```elisp
(setq cquery-extra-init-params '(:enableComments 2 :cacheFormat "msgpack"))
```

Refer to <https://github.com/jacobdufault/cquery/wiki/Emacs> for details.

![with lsp-ui-doc, enableComments](https://camo.githubusercontent.com/fe1e12f9be72c2295d732d6265b42bde0d121ee8/68747470733a2f2f707470622e70772f5a6275462e6a7067)
![semantic highlighting](https://user-images.githubusercontent.com/320163/34711725-5b7b399c-f55b-11e7-93dc-0631aa154ce7.png)
![with company-lsp](https://ptpb.pw/lDaw.jpg)
![with helm-xref, approximate workspace/symbol search](https://ptpb.pw/KOKn.jpg)

## License

[MIT](http://opensource.org/licenses/MIT)

[![MELPA](https://melpa.org/packages/cquery-badge.svg)](https://melpa.org/#/cquery)

# emacs-cquery

emacs-cquery is a client for [cquery](https://github.com/jacobdufault/cquery), a low-latency language server supporting multi-million line C++ code-bases, powered by libclang.

It leverages [lsp-mode](https://github.com/emacs-lsp/lsp-mode), but also provides some cquery extensions to LSP:

* semantic highlighting
* inactive region (e.g. a `#if false` region)
* cross references: `$cquery/base` `$cquery/callers` `$cquery/derived` `$cquery/vars`

More on <https://github.com/cquery-project/cquery/wiki/Emacs>

## Quickstart

```elisp
(require 'cquery)
(setq cquery-executable "/path/to/cquery/build/release/bin/cquery")
;; (setq cquery-executable "/path/to/cquery-install-prefix/bin/cquery")
```

To enable comments and use Message Pack for cache files (which are stored in `cacheDirectory`):

```elisp
(setq cquery-extra-init-params '(:index (:comments 2) :cacheFormat "msgpack"))
```

Refer to <https://github.com/jacobdufault/cquery/wiki/Emacs> for details.

![with lsp-ui-doc, enableComments](https://camo.githubusercontent.com/fe1e12f9be72c2295d732d6265b42bde0d121ee8/68747470733a2f2f707470622e70772f5a6275462e6a7067)
![references + hydra](https://ptpb.pw/fhWh.jpg)
![with company-lsp](https://ptpb.pw/lDaw.jpg)
![with helm-xref, approximate workspace/symbol search](https://ptpb.pw/KOKn.jpg)

`$cquery/memberHierarchy`
![$cquery/memberHierarchy](https://ptpb.pw/iOSt.gif)

`$cquery/callHierarchy` (caller hierarchy and callee hierarchy)
![$cquery/callHierarchy](https://ptpb.pw/GKJw.gif)

## License

[MIT](http://opensource.org/licenses/MIT)

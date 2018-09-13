[![MELPA](https://melpa.org/packages/ccls-badge.svg)](https://melpa.org/#/ccls)

# emacs-ccls

emacs-ccls is a client for [ccls](https://github.com/MaskRay/ccls), a C/C++/Objective-C language server supporting multi-million line C++ code-bases, powered by libclang.

It leverages [lsp-mode](https://github.com/emacs-lsp/lsp-mode), but also provides some ccls extensions to LSP:

* semantic highlighting
* skipped ranges (e.g. a `#if false` region)
* cross references: `$ccls/inheritance` `$ccls/call` `$ccls/vars`

More on <https://github.com/MaskRay/ccls/wiki/Emacs>

## Quickstart

```elisp
(require 'ccls)
(setq ccls-executable "/path/to/ccls/Release/ccls")
```

Refer to <https://github.com/MaskRay/ccls/wiki/Emacs> for details.

![with lsp-ui-doc, enableComments](https://camo.githubusercontent.com/fe1e12f9be72c2295d732d6265b42bde0d121ee8/68747470733a2f2f707470622e70772f5a6275462e6a7067)
![references + hydra](https://ptpb.pw/fhWh.jpg)
![with company-lsp](https://ptpb.pw/lDaw.jpg)
![with helm-xref, approximate workspace/symbol search](https://ptpb.pw/KOKn.jpg)

`$ccls/member`
![$ccls/member](https://ptpb.pw/iOSt.gif)

`$ccls/call` (caller/callee, with hierarchical view)

### `ccls-navigate`

https://github.com/MaskRay/ccls/wiki/Emacs#ccls-navigate

![ccls-navigate](https://camo.githubusercontent.com/7d9c5e9ad1297fa493d1256f9bc9824f56806d23/68747470733a2f2f707470622e70772f4858744d2e676966)

## License

[MIT](http://opensource.org/licenses/MIT)

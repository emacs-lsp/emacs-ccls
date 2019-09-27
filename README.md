[![MELPA](https://melpa.org/packages/ccls-badge.svg)](https://melpa.org/#/ccls)

# emacs-ccls

emacs-ccls is a client for [ccls](https://github.com/MaskRay/ccls), a C/C++/Objective-C language server supporting multi-million line C++ code-bases, powered by libclang.

It leverages [lsp-mode](https://github.com/emacs-lsp/lsp-mode), but also provides some ccls extensions to LSP:

* semantic highlighting
* skipped ranges (e.g. a `#if false` region)
* cross references: `$ccls/inheritance` `$ccls/call` `$ccls/vars`

## Quickstart

```elisp
(require 'ccls)
(setq ccls-executable "/path/to/ccls/Release/ccls")
```

Refer to <https://github.com/MaskRay/ccls/wiki/lsp-mode> for details.

<img src="https://raw.githubusercontent.com/MaskRay/ccls-static/master/emacs-ccls/lsp-ui-doc.webp" title="lsp-ui-doc" width=70%>
<img src="https://raw.githubusercontent.com/MaskRay/ccls-static/master/emacs-ccls/reference-hydra.webp" title="references+hydra" width=70%>
<img src="https://raw.githubusercontent.com/MaskRay/ccls-static/master/emacs-ccls/company-lsp.webp" title="company-lsp" width=50%>
<img src="https://raw.githubusercontent.com/MaskRay/ccls-static/master/emacs-ccls/ccls-member.webp" title="$ccls/member" width=70%>

`$ccls/call` (caller/callee, with hierarchical view)

### `ccls-navigate`

https://github.com/MaskRay/ccls/wiki/lsp-mode#ccls-navigate

![ccls-navigate](https://raw.githubusercontent.com/MaskRay/ccls-static/master/emacs-ccls/ccls-navigate.gif)

## License

[MIT](http://opensource.org/licenses/MIT)

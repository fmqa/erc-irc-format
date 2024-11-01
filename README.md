# erc-irc-format.el

A [transient.el](https://www.gnu.org/software/emacs/manual/html_mono/transient.html)-based UI for [IRC formatting control codes](https://modern.ircdocs.horse/formatting.html), for usage within Erc.

# Requirements

* Emacs 29+
* Erc 5.6+
* (Recommended) The latest version of transient – on Emacs 29+, the built-in version can be upgraded via `C-u M-x package-install RET transient RET`.

# Installation

Load `erc-irc-format.el` into a buffer and execute `M-x install-package-from-buffer`, then insert the following into your `init.el`:

```
(require 'erc-irc-format)
(define-key erc-mode-map (kbd "C-c q") #'erc-irc-format)
```

Alternatively, if you are using `use-package`:

(use-package erc
   :bind (:map erc-mode-map ("C-c q" . erc-irc-format)))

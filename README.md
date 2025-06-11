# erc-irc-format.el

A [transient.el](https://www.gnu.org/software/emacs/manual/html_mono/transient.html)-based UI for [IRC formatting control codes](https://modern.ircdocs.horse/formatting.html), for usage within Erc.

![Foreground selection](.assets/0-fg.png?raw=true "Foreground color selection")
![Background selection](.assets/1-bg.png?raw=true "Background color selection")

# Requirements

* Emacs 30+
* Erc 5.6+
* (Recommended) The latest version of transient – on Emacs 29+, the built-in version can be upgraded via `C-u M-x package-install RET transient RET`.

# Installation

```
(use-package erc-irc-format
  :defer t
  :vc (:url "https://github.com/fmqa/erc-irc-format.git" :branch "main")
  :bind (:map erc-mode-map ("C-c q" . erc-irc-format)))
```

The package also includes an Erc module `erc-normalize` which enables yanking formatted text into the Erc input line. The module may be enabled via customizing `erc-modules` or by executing `M-x erc-normalize-enable`.

# Usage

## Commands

* `M-x erc-irc-format` / _C-c q_: Open up a transient menu for formatting. If a region is active, that region is wrapped with the selected formatting code. Otherwise, the formatting code is inserted at point.
* `M-x erc-normalize-mode` / `M-x erc-normalize-enable` / `M-x erc-normalize-disable`: Enable or disable yank normalization in Erc - this normalizes/strips text properties from text inputted at the Erc input prompt, automatically translating them to formatting codes.

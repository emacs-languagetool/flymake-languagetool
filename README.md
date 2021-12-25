[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/flymake-languagetool-badge.svg)](https://melpa.org/#/flymake-languagetool)
[![MELPA Stable](https://stable.melpa.org/packages/flymake-languagetool-badge.svg)](https://stable.melpa.org/#/flymake-languagetool)

<img align="right" src="./etc/logo.png" with="138" height="46">

# flymake-languagetool
> Flymake support for LanguageTool.

[![CI](https://github.com/emacs-languagetool/flymake-languagetool/actions/workflows/test.yml/badge.svg)](https://github.com/emacs-languagetool/flymake-languagetool/actions/workflows/test.yml)

## :floppy_disk: Installation

The instruction to use this plugin.

1. Download LanguageTool from https://languagetool.org/download/ and
   extract on to your local machine (skip this step if using a remote
   server).
2. Consider adding the following snippet to your configuration.

```el
(use-package flymake-languagetool
  :ensure t
  :hook ((text-mode       . flymake-languagetool-load)
         (latex-mode      . flymake-languagetool-load)
         (org-mode        . flymake-languagetool-load)
         (markdown-mode   . flymake-languagetool-load))
  :init
  ;; Remote server config with LanguageTool's free API
  ;; (setq flymake-languagetool-url "https://api.languagetool.org")
  ;; (setq flymake-languagetool-server-port nil)
  ;; (setq flymake-languagetool-server-jar nil)

  ;; Local server config
  (setq flymake-languagetool-server-jar "path/to/LanguageTool-X.X/languagetool-server.jar"))
```

3. :tada: Done! Now open a text file and hit `M-x flymake-mode`!

otherwise you can call `flymake-languagetool-maybe-load` like the snippet below.

```el
(add-hook 'find-file-hook 'flymake-languagetool-maybe-load)
```

## :wrench: Configuration

* `flymake-languagetool-server-jar`
* `flymake-languagetool-active-modes`
* `flymake-languagetool-check-spelling`
* `flymake-languagetool-language` (Default `"en-US"`)

### :book: Spellchecking

LanguageTool’s spellchecking is disabled by default. If
`flymake-languagetool-check-spelling` is non-nil LanguageTool will check
for spelling mistakes.

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!

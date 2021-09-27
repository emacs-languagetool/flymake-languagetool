[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/flymake-languagetool-badge.svg)](https://melpa.org/#/flymake-languagetool)
[![MELPA Stable](https://stable.melpa.org/packages/flymake-languagetool-badge.svg)](https://stable.melpa.org/#/flymake-languagetool)

<img align="right" src="./etc/logo.png" with="138" height="46">

# flymake-languagetool
> Flymake support for LanguageTool.

[![CI](https://github.com/emacs-languagetool/flymake-languagetool/actions/workflows/test.yml/badge.svg)](https://github.com/emacs-languagetool/flymake-languagetool/actions/workflows/test.yml)

## :floppy_disk: Installation

The instruction to use this plugin.

1. Download LanguageTool from https://languagetool.org/download/.
2. Extract on to your local machine.
3. Consider adding the following snippet to your configuration.

```el
(use-package flymake-languagetool
  :ensure t
  :hook ((text-mode       . flymake-languagetool-load)
         (latex-mode      . flymake-languagetool-load)
         (org-mode        . flymake-languagetool-load)
         (markdown-mode   . flymake-languagetool-load))
  :init
  (setq flymake-languagetool-commandline-jar "path/to/LanguageTool-X.X/languagetool-commandline.jar"))
```

4. :tada: Done! Now open a text file and hit `M-x flymake-mode`!

otherwise you can call `flymake-languagetool-maybe-load` like the snippet below.

```el
(add-hook 'find-file-hook 'flymake-languagetool-maybe-load)
```

## :wrench: Configuration

* `flymake-languagetool-commandline-jar`
* `flymake-languagetool-active-modes`
* `flymake-languagetool-language` (Default `"en-US"`)

## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!

[![Build Status](https://travis-ci.com/emacs-languagetool/flymake-languagetool.svg?branch=master)](https://travis-ci.com/emacs-languagetool/flymake-languagetool)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# flymake-languagetool
> Flymake support for LanguageTool.

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

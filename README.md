[![Build Status](https://travis-ci.com/emacs-languagetool/flymake-languagetool.svg?branch=master)](https://travis-ci.com/emacs-languagetool/flymake-languagetool)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# flymake-languagetool
> Flymake support for LanguageTool.

## Usage

To enable this package, simply add loading to your config like the code below.

```el
(require 'flymake-languagetool)
```

You can generally call `flymake-languagetool-load` to any `mode-hook` for
LanguageTool to start working.

```el
(add-hook 'text-mode-hook 'flymake-languagetool-load)
(add-hook 'latex-mode-hook 'flymake-languagetool-load)
(add-hook 'org-mode-hook 'flymake-languagetool-load)
(add-hook 'markdown-mode-hook 'flymake-languagetool-load)
```

otherwise you can call `flymake-languagetool-maybe-load` like the snippet below.

```el
(add-hook 'find-file-hook 'flymake-languagetool-maybe-load)
```

## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!

;;; flymake-languagetool.el --- Flymake support for LanguageTool  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-04-02 23:22:37

;; Authors: Shen, Jen-Chieh <jcs090218@gmail.com>, Trey Peacock <git@treypeacock.com>
;; Description: Flymake support for LanguageTool
;; Keyword: grammar check
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (s "1.9.0"))
;; URL: https://github.com/emacs-languagetool/flymake-languagetool

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Flymake support for LanguageTool.
;;

;;; Code:

(require 'url)
(require 'json)
(require 'flymake)

;; Dynamically bound.
(defvar url-http-end-of-headers)


(defgroup flymake-languagetool nil
  "Flymake support for LanguageTool."
  :prefix "flymake-languagetool-"
  :group 'flymake
  :link '(url-link :tag "Github" "https://github.com/emacs-languagetool/flymake-languagetool"))

(defcustom flymake-languagetool-active-modes
  '(text-mode latex-mode org-mode markdown-mode message-mode)
  "List of major mode that work with LanguageTool."
  :type 'list
  :group 'flymake-languagetool)

(defcustom flymake-languagetool-url nil
  "The URL for the LanguageTool API we should connect to."
  :type '(choice (const :tag "Auto" nil)
                 (string :tag "URL"))
  :group 'flymake-languagetool)

(defcustom flymake-languagetool-server-jar nil
  "The path of languagetool-server.jar.

The server will be automatically started if specified.  Set to
nil if you’re going to connect to a remote LanguageTool server,
or plan to start a local server some other way."
  :type '(choice (const :tag "Off" nil)
                 (file :tag "Filename" :must-match t))
  :link '(url-link :tag "LanguageTool embedded HTTP Server"
           "https://dev.languagetool.org/http-server.html")
  :group 'flymake-languagetool)

(defcustom flymake-languagetool-server-command
  '("java" "-cp" flymake-languagetool-server-jar "org.languagetool.server.HTTPServer"
    "--port" flymake-languagetool-server-port)
  "Command used to start LanguageTool server.
This should be a list of strings or symbols that should evaluate to strings."
  :type '(repeat (choice string symbol))
  :link '(url-link :tag "LanguageTool embedded HTTP Server"
           "https://dev.languagetool.org/http-server.html")
  :group 'flymake-languagetool)

(defcustom flymake-languagetool-server-port 8081
  "The port on which an automatically started LanguageTool server should listen."
  :type 'integer
  :link '(url-link :tag "LanguageTool embedded HTTP Server"
           "https://dev.languagetool.org/http-server.html")
  :group 'flymake-languagetool)

(defcustom flymake-languagetool-language "en-US"
  "The language code of the text to check."
  :type '(string :tag "Language")
  :safe #'stringp
  :group 'flymake-languagetool)
(make-variable-buffer-local 'flymake-languagetool-language)

(defcustom flymake-languagetool-check-spelling nil
  "If non-nil, LanguageTool will check spelling."
  :type '(string :tag "Language")
  :safe #'stringp
  :group 'flymake-languagetool)

(defcustom flymake-languagetool-check-params ()
  "Extra parameters to pass with LanguageTool check requests."
  :type '(alist :key-type string :value-type string)
  :link '(url-link
          :tag "LanguageTool API"
          "https://languagetool.org/http-api/swagger-ui/#!/default/post_check")
  :group 'flymake-languagetool)

(defvar flymake-languagetool--spelling-rules
  '("HUNSPELL_RULE"
    "HUNSPELL_RULE_AR"
    "MORFOLOGIK_RULE_AST"
    "MORFOLOGIK_RULE_BE_BY"
    "MORFOLOGIK_RULE_BR_FR"
    "MORFOLOGIK_RULE_CA_ES"
    "MORFOLOGIK_RULE_DE_DE"
    "MORFOLOGIK_RULE_EL_GR"
    "MORFOLOGIK_RULE_EN"
    "MORFOLOGIK_RULE_EN_AU"
    "MORFOLOGIK_RULE_EN_CA"
    "MORFOLOGIK_RULE_EN_GB"
    "MORFOLOGIK_RULE_EN_NZ"
    "MORFOLOGIK_RULE_EN_US"
    "MORFOLOGIK_RULE_EN_ZA"
    "MORFOLOGIK_RULE_ES"
    "MORFOLOGIK_RULE_GA_IE"
    "MORFOLOGIK_RULE_IT_IT"
    "MORFOLOGIK_RULE_LT_LT"
    "MORFOLOGIK_RULE_ML_IN"
    "MORFOLOGIK_RULE_NL_NL"
    "MORFOLOGIK_RULE_PL_PL"
    "MORFOLOGIK_RULE_RO_RO"
    "MORFOLOGIK_RULE_RU_RU"
    "MORFOLOGIK_RULE_RU_RU_YO"
    "MORFOLOGIK_RULE_SK_SK"
    "MORFOLOGIK_RULE_SL_SI"
    "MORFOLOGIK_RULE_SR_EKAVIAN"
    "MORFOLOGIK_RULE_SR_JEKAVIAN"
    "MORFOLOGIK_RULE_TL"
    "MORFOLOGIK_RULE_UK_UA"
    "SYMSPELL_RULE")
  "LanguageTool rules for checking of spelling.
These rules will be enabled if `flymake-languagetool-check-spelling' is
non-nil.")

(defvar-local flymake-languagetool--source-buffer nil
  "Current buffer we are currently using for grammar check.")

(defvar flymake-languagetool--report-fnc nil
  "Record report function/execution.")

;;; Util

(defun flymake-languagetool--check-all (errors source-buffer)
  "Check grammar ERRORS for SOURCE-BUFFER document."
  (let (check-list)
    (dolist (error errors)
      (let-alist error
        (push (flymake-make-diagnostic
               source-buffer
               (+ .offset 1)
               (+ .offset .length 1)
               :warning
               (concat .message " [LanguageTool]"))
              check-list)))
    check-list))

(defun flymake-languagetool--output-to-errors (output source-buffer)
  "Parse the JSON data from OUTPUT of LanguageTool."
  (let* ((json-array-type 'list)
         (full-results (json-read-from-string output))
         (errors (cdr (assoc 'matches full-results))))
    (flymake-languagetool--check-all errors source-buffer)))

(defun flymake-languagetool--handle-finished (status source-buffer report-fn)
  "Callback function for LanguageTool process for SOURCE-BUFFER.
STATUS provided from `url-retrieve'."
  (when-let ((err (plist-get status :error)))
    (kill-buffer)
    (funcall report-fn :panic :explanation (error-message-string err))
    (error (error-message-string err)))
  (set-buffer-multibyte t)
  (goto-char url-http-end-of-headers)
  (let* ((output (buffer-substring (point) (point-max)))
         (errors (flymake-languagetool--output-to-errors output source-buffer))
         (region (with-current-buffer source-buffer
                   (cons (point-min) (point-max)))))
    (funcall report-fn errors :region region)))

(defun flymake-languagetool--expand-server-command ()
  "Return result of evaluating symbols in `flymake-languagetool-server-command'."
  (mapcar (lambda (el)
            (if (symbolp el) (format "%s" (symbol-value el)) el))
          flymake-languagetool-server-command))

(defun flymake-languagetool--start-server ()
  "Start the LanguageTool server if we didn’t already."
  (unless (process-live-p (get-process "languagetool-server"))
    (let ((process
           (apply #'start-process
                  "languagetool-server"
                  "*LanguageTool server*"
                  (flymake-languagetool--expand-server-command))))
      (set-process-query-on-exit-flag process nil)
      (while
          (with-current-buffer (process-buffer process)
            (goto-char (point-min))
            (unless (re-search-forward " Server started$" nil t)
              (accept-process-output process 1)
              (process-live-p process)))))))

;;; Flymake

(defun flymake-languagetool--start ()
  "Run LanguageTool on the current buffer's contents."
  (unless flymake-languagetool-url
    (flymake-languagetool--start-server))
  (let* ((report-fn flymake-languagetool--report-fnc)
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (source-buffer (current-buffer))
         (params (list
                  (list "text" (with-current-buffer source-buffer (buffer-string)))
                  (list "language" flymake-languagetool-language)
                  (unless flymake-languagetool-check-spelling
                    (list "disabledRules" (string-join flymake-languagetool--spelling-rules
                                                       ",")))))
         (url-request-data (url-build-query-string params)))
    (url-retrieve
     (concat (or flymake-languagetool-url
                 (format "http://localhost:%s"
                         flymake-languagetool-server-port))
             "/v2/check")
     #'flymake-languagetool--handle-finished
     (list source-buffer report-fn) t)))


(defun flymake-languagetool--checker (report-fn &rest _args)
  "Diagnostic checker function with REPORT-FN."
  (setq flymake-languagetool--report-fnc report-fn)
  (setq flymake-languagetool--source-buffer (current-buffer))
  (flymake-languagetool--start))

;;; Entry

;;;###autoload
(defun flymake-languagetool-load ()
  "Convenience function to setup flymake-languagetool.
This adds the language-tool checker to the list of flymake diagnostic functions."
  (add-hook 'flymake-diagnostic-functions #'flymake-languagetool--checker nil t))

;;;###autoload
(defun flymake-languagetool-maybe-load ()
  "Call `flymake-languagetool-load' if this file appears to be check for grammar."
  (interactive)
  (when (memq major-mode flymake-languagetool-active-modes)
    (flymake-languagetool-load)))

(provide 'flymake-languagetool)
;;; flymake-languagetool.el ends here

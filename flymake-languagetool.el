;;; flymake-languagetool.el --- Flymake support for LanguageTool  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025  Shen, Jen-Chieh
;; Created date 2021-04-02 23:22:37

;; Authors: Shen, Jen-Chieh <jcs090218@gmail.com>, Trey Peacock <git@treypeacock.com>
;; URL: https://github.com/emacs-languagetool/flymake-languagetool
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (compat "29.1.4.4"))
;; Keywords: convenience grammar check

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

(require 'compat)
(require 'seq)
(eval-when-compile
  (require 'cl-lib))
(require 'url)
(require 'flymake)

;; Either use the built-in JSON support or import the `json' library, defining a
;; compatibility function so we can use the best supported JSON parser.
(defalias 'flymake-languagetool--parse-json
  (if (and (fboundp 'json-parse-string)
           (fboundp 'json-available-p)
           (json-available-p))
      (lambda (string)
        "Parse a json STRING."
        (json-parse-string string
                           :array-type 'list
                           :object-type 'alist
                           :false-object :json-false
                           :null-object nil))
    (require 'json)
    'json-read-string))

;; Dynamically bound.
(defvar url-http-end-of-headers)

(defgroup flymake-languagetool nil
  "Flymake support for LanguageTool."
  :prefix "flymake-languagetool-"
  :group 'flymake
  :link '(url-link :tag "Github"
                   "https://github.com/emacs-languagetool/flymake-languagetool"))

(defcustom flymake-languagetool-active-modes
  '(text-mode latex-mode org-mode markdown-mode message-mode)
  "List of major mode that work with LanguageTool."
  :type '(repeat symbol)
  :group 'flymake-languagetool)

(defcustom flymake-languagetool-ignore-faces-alist
  '((org-mode . (org-code org-verbatim
                          org-block font-lock-comment-face
                          org-block-begin-line org-block-end-line
                          org-special-keyword org-table org-tag))
    (message-mode . (message-header-cc
                     message-header-to
                     message-header-other
                     message-mml
                     message-cited-text
                     message-cited-text-1
                     message-cited-text-2
                     message-cited-text-3
                     message-cited-text-4))
    (markdown-mode . (markdown-code-face
                      markdown-markup-face
                      markdown-inline-code-face markdown-pre-face
                      markdown-url-face markdown-plain-url-face
                      markdown-math-face markdown-html-tag-name-face
                      markdown-html-tag-delimiter-face
                      markdown-html-attr-name-face
                      markdown-html-attr-value-face
                      markdown-html-entity-face)))
  "Filters out errors if they are of fortified with faces in this alist.
It is an alist of (major-mode . faces-to-ignore)"
  :type '(alist :key-type symbol
                :value-type (repeat symbol))
  :group 'flymake-languagetool)

(defcustom flymake-languagetool-url nil
  "The URL for the LanguageTool API we should connect to."
  :type '(choice (const :tag "Auto" nil)
                 (string :tag "URL"))
  :group 'flymake-languagetool)

(defcustom flymake-languagetool-api-username nil
  "The username for accessing the Premium LanguageTool API."
  :type 'string
  :group 'flymake-languagetool)

(defcustom flymake-languagetool-api-key nil
  "The API Key for accessing the Premium LanguageTool API."
  :type 'string
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

(defcustom flymake-languagetool-server-port "8081"
  "Port used to make api url requests on local server."
  :type 'string
  :link '(url-link :tag "LanguageTool embedded HTTP Server"
                   "https://dev.languagetool.org/http-server.html")
  :group 'flymake-languagetool)

(defcustom flymake-languagetool-server-command ()
  "Custom command to start LanguageTool server.
If non-nil, this list of strings replaces the standard java cli command."
  :type '(repeat string)
  :group 'flymake-languagetool)

(defcustom flymake-languagetool-server-args ()
  "Extra arguments to pass when starting the LanguageTool server."
  :type '(repeat string)
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
  :type 'boolean
  :safe #'booleanp
  :group 'flymake-languagetool)

(defcustom flymake-languagetool-check-params ()
  "Extra parameters to pass with LanguageTool check requests."
  :type '(alist :key-type string :value-type string)
  :link '(url-link :tag "LanguageTool API"
                   "https://languagetool.org/http-api/swagger-ui/#!/default/post_check")
  :group 'flymake-languagetool)

(defcustom flymake-languagetool-spelling-rules
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
These rules will be enabled if `flymake-languagetool-check-spelling' is non-nil."
  :type '(repeat string)
  :group 'flymake-languagetool)

(defcustom flymake-languagetool-disabled-rules '()
  "LanguageTool rules to be disabled by default."
  :type '(repeat string)
  :group 'flymake-languagetool)

(defcustom flymake-languagetool-disabled-categories '()
  "LanguageTool categories to be disabled by default."
  :type '(repeat string)
  :group 'flymake-languagetool)

(defcustom flymake-languagetool-use-categories t
  "Report errors with LanguageTool Category."
  :type 'boolean
  :safe #'booleanp
  :group 'flymake-languagetool)

(defvar-local flymake-languagetool--proc-buf nil
  "Current process we are currently using for grammar check.")

(defvar flymake-languagetool--local nil
  "Can we reach the local LanguageTool server API?")

(defconst flymake-languagetool-category-map
  '(("CASING"            . :casing)
    ("COLLOQUIALISMS"    . :colloquialisms)
    ("COMPOUNDING"       . :compounding)
    ("CONFUSED_WORDS"    . :confused-words)
    ("FALSE_FRIENDS"     . :false-friends)
    ("GENDER_NEUTRALITY" . :gender-neutrality)
    ("GRAMMAR"           . :grammar)
    ("MISC"              . :misc)
    ("PLAIN_ENGLISH"     . :plain-english)
    ("PUNCTUATION"       . :punctuation)
    ("REDUNDANCY"        . :redundancy)
    ("REGIONALISMS"      . :regionalisms)
    ("REPETITIONS"       . :repetitions)
    ("REPETITIONS_STYLE" . :repetitions-style)
    ("SEMANTICS"         . :semantics)
    ("STYLE"             . :style)
    ("TYPOGRAPHY"        . :typography)
    ("TYPOS"             . :typos)
    ("WIKIPEDIA"         . :wikipedia))
  "LanguageTool category mappings.

See https://languagetool.org/development/api/org/languagetool/rules/Categories.html.")

;;
;;; Util

(defun flymake-languagetool--category-setup ()
  "Setup LanguageTool categories as Flymake types."
  (cl-loop for (n . key) in flymake-languagetool-category-map
           for name = (downcase (string-replace "_" "-" n))
           for cat = (intern (format "flymake-languagetool-%s" name))
           do
           (put key 'flymake-category cat)
           (put cat 'face 'flymake-warning)
           (put cat 'flymake-bitmap 'flymake-warning-bitmap)
           (put cat 'severity (warning-numeric-level :warning))
           (put cat 'mode-line-face 'compilation-warning)
           (put cat 'echo-face 'flymake-warning-echo)
           (put cat 'eol-face 'flymake-warning-echo-at-eol)
           (put cat 'flymake-type-name name)))

(when flymake-languagetool-use-categories
  (flymake-languagetool--category-setup))

;; Ignore some faces
(defun flymake-languagetool--ignore-at-pos-p (pos src-buf
                                                  faces-to-ignore)
  "Return non-nil if faces at POS in SRC-BUF intersect FACES-TO-IGNORE."
  (let ((x (get-text-property pos 'face src-buf)))
    (cl-loop
     for face in (ensure-list x)
     when (memq face faces-to-ignore)
     return t)))

(defun flymake-languagetool--ignored-faces ()
  "Return the faces that should be ignored in the current buffer."
  (cl-loop
   for (mode . faces) in flymake-languagetool-ignore-faces-alist
   when (derived-mode-p mode)
   append (ensure-list faces)))

(defun flymake-languagetool--pos-to-point (buf offset pos)
  "Search forward in BUF for the specified text position POS from OFFSET.
This function correctly handles emoji which count as two characters."
  (let (case-fold-search)
    (with-current-buffer buf
      (save-excursion
        (setq pos (+ offset pos))
        (goto-char offset)
        ;; code points in the "supplementary place" use two code units
        (while (and (< (point) pos)
                    (re-search-forward (rx (any (#x010000 .  #x10ffff))) pos t))
          (setq pos (1- pos)))
        pos))))

(defun flymake-languagetool--check-all (errors source-buffer)
  "Check grammar ERRORS for SOURCE-BUFFER document."
  (let ((faces (with-current-buffer source-buffer
                 (flymake-languagetool--ignored-faces)))
        check-list)
    (dolist (error errors)
      (let-alist error
        (let* ((beg (flymake-languagetool--pos-to-point source-buffer (point-min) .offset))
               (end (flymake-languagetool--pos-to-point source-buffer beg .length)))
          (unless (and faces (flymake-languagetool--ignore-at-pos-p beg source-buffer faces))
            (push (flymake-make-diagnostic
                   source-buffer
                   beg end
                   (if flymake-languagetool-use-categories
                       (map-elt flymake-languagetool-category-map
                                .rule.category.id)
                     :warning)
                   (concat .message " [LanguageTool]")
                   ;; add text property for suggested replacements
                   `((suggestions . (,@(seq-map (lambda (rep)
                                                  (car (map-values rep)))
                                                .replacements)))
                     (rule-id . ,.rule.id)
                     (rule-desc . ,.rule.description)
                     (type . ,.rule.issueType)
                     (category . ,.rule.category.id)))
                  check-list)))))
    check-list))

(defun flymake-languagetool--output-to-errors (output source-buffer)
  "Parse the JSON data from OUTPUT of LanguageTool analysis of SOURCE-BUFFER."
  (let* ((full-results (flymake-languagetool--parse-json output))
         (errors (cdr (assoc 'matches full-results))))
    (flymake-languagetool--check-all errors source-buffer)))

(defun flymake-languagetool--handle-finished (status source-buffer report-fn)
  "Callback function for LanguageTool process for SOURCE-BUFFER.
STATUS provided from `url-retrieve'."
  (let* ((err (plist-get status :error))
         (c-buf (current-buffer))
         (proc-buf (buffer-local-value 'flymake-languagetool--proc-buf
                                       source-buffer))
         (proc-current (equal c-buf proc-buf)))
    (cond
     ((and proc-current err)
      ;; Ignore errors about deleted processes since they are obsolete
      ;; calls deleted by `flymake-languagetool--check'
      (unless (equal "deleted" (string-trim (nth 2 err)))
        (with-current-buffer source-buffer
          ;; for some reason the 2nd element in error list is a
          ;; symbol. This needs to be changed to string to reflect in
          ;; `error-message-string'
          (setf (nth 1 err) (symbol-name (nth 1 err)))
          (funcall report-fn :panic :explanation
                   (format "%s: %s" c-buf (error-message-string err))))))
     ((and proc-current url-http-end-of-headers)
      (let ((output (save-restriction
                      (set-buffer-multibyte t)
                      (goto-char url-http-end-of-headers)
                      (buffer-substring (point) (point-max)))))
        (with-current-buffer source-buffer
          (funcall report-fn
                   (flymake-languagetool--output-to-errors output source-buffer)
                   :region (cons (point-min) (point-max))))))
     ((not proc-current)
      (with-current-buffer source-buffer
        (flymake-log :warning "Skipping an obsolete check"))))
    (kill-buffer c-buf)))

(defun flymake-languagetool--check (report-fn text)
  "Run LanguageTool on TEXT from current buffer's contento.
The callback function will reply with REPORT-FN."
  (when-let* ((buf flymake-languagetool--proc-buf))
    ;; need to check if buffer has ongoing process or else we may
    ;; potentially delete the wrong one.
    (when-let* ((process (get-buffer-process buf)))
      (delete-process process))
    (setf flymake-languagetool--proc-buf nil))
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (source-buffer (current-buffer))
         (disabled-cats
          (string-join flymake-languagetool-disabled-categories ","))
         (disabled-rules
          (string-join (append flymake-languagetool-disabled-rules
                               (unless flymake-languagetool-check-spelling
                                 flymake-languagetool-spelling-rules))
                       ","))
         (params (list (list "text" text)
                       (list "language" flymake-languagetool-language)
                       (unless (string-empty-p disabled-rules)
                         (list "disabledRules" disabled-rules))
                       (unless (string-empty-p disabled-cats)
                         (list "disabledCategories" disabled-cats))
                       (when flymake-languagetool-api-username
                         (list "username" flymake-languagetool-api-username))
                       (when flymake-languagetool-api-key
                         (list "apiKey" flymake-languagetool-api-key))))
         (url-request-data (url-build-query-string params nil t)))
    (if (flymake-languagetool--reachable-p)
        (setq flymake-languagetool--proc-buf
              (url-retrieve
               (concat (or flymake-languagetool-url
                           (format "http://localhost:%s"
                                   flymake-languagetool-server-port))
                       "/v2/check")
               #'flymake-languagetool--handle-finished
               (list source-buffer report-fn) t))
      ;; can't reach LanguageTool API, try again. TODO:
      (funcall report-fn :panic :explanation
               (format "Cannot reach LanguageTool URL: %s"
                       flymake-languagetool-url)))))

(defun flymake-languagetool--reachable-p ()
  "TODO: Document this."
  (let ((res (or flymake-languagetool--local
                 (condition-case nil
                     (url-retrieve-synchronously
                      (concat (or flymake-languagetool-url
                                  (format "http://localhost:%s"
                                          flymake-languagetool-server-port))
                              "/v2/languages")
                      t)
                   (file-error nil)))))
    (when (buffer-live-p res)
      (kill-buffer res)
      (setq res t))
    res))

(defun flymake-languagetool--start-server (report-fn)
  "Start the LanguageTool server if we didn’t already.
Once started call `flymake-languagetool' checker with REPORT-FN."
  (let* ((source (current-buffer))
         (cmd (or flymake-languagetool-server-command
                  (list "java" "-cp" flymake-languagetool-server-jar
                        "org.languagetool.server.HTTPServer"
                        "--port" flymake-languagetool-server-port))))
    (make-process
     :name "languagetool-server" :noquery t :connection-type 'pipe
     :buffer " *LanguageTool server*"
     :command (append cmd flymake-languagetool-server-args)
     :filter
     (lambda (proc string)
       (funcall #'internal-default-process-filter proc string)
       (when (string-match ".*Server started\n$" string)
         (with-current-buffer source
           (setq flymake-languagetool--local t)
           (flymake-languagetool--checker report-fn))
         (set-process-filter proc nil)))
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (setq flymake-languagetool--local nil)
         (delete-process proc)
         (kill-buffer (process-buffer proc)))))))

(defun flymake-languagetool--checker (report-fn &rest _args)
  "Diagnostic checker function with REPORT-FN."
  (let ((text (buffer-substring-no-properties
               (point-min) (point-max))))
    (cond
     ((flymake-languagetool--reachable-p)
      (flymake-languagetool--check report-fn text))
     ((or flymake-languagetool-server-command flymake-languagetool-server-jar)
      (flymake-languagetool--start-server report-fn))
     (t (funcall report-fn :panic :explanation
                 (format "Cannot reach LanguageTool URL: %s"
                         flymake-languagetool-url))))))

(defun flymake-languagetool--overlay-p (overlay)
  "Return t if OVERLAY is a `flymake-languagetool' diagnostic overlay."
  (when-let* ((diag (overlay-get overlay 'flymake-diagnostic))
              (backend (flymake-diagnostic-backend diag)))
    (eq backend 'flymake-languagetool--checker)))

(defun flymake-languagetool--ovs (&optional format)
  "List of all `flymake-languagetool' diagnostic overlays.
Optionally provide pretty FORMAT for each overlay."
  (let* ((lt-ovs (seq-filter #'flymake-languagetool--overlay-p
                             (overlays-in (point-min) (point-max))))
         (ovs (seq-sort-by #'overlay-start #'< lt-ovs)))
    (if format
        (seq-map
         (lambda (ov) (cons (format "%s: %s"
                                    (line-number-at-pos (overlay-start ov))
                                    (flymake-diagnostic-text
                                     (overlay-get ov 'flymake-diagnostic)))
                            ov))
         ovs)
      ovs)))

(defvar-local flymake-languagetool-current-cand nil
  "Current overlay candidate.")

(defun flymake-languagetool--ov-at-point ()
  "Return `flymake-languagetool' overlay at point."
  (setq flymake-languagetool-current-cand
        (car (seq-filter #'flymake-languagetool--overlay-p
                         (overlays-at (point))))))

(defun flymake-languagetool--suggestions ()
  "Show corrections suggested from LanguageTool."
  (overlay-put flymake-languagetool-current-cand 'face 'isearch)
  (let ((sugs (map-elt (flymake-diagnostic-data
                        (overlay-get flymake-languagetool-current-cand
                                     'flymake-diagnostic))
                       'suggestions)))
    (seq-remove #'null `(,@sugs "Ignore Rule" "Ignore Category"))))

(defun flymake-languagetool--clean-overlay ()
  "Remove highlighting of current candidate."
  (ignore-errors
    (overlay-put flymake-languagetool-current-cand 'face 'flymake-warning))
  (setq flymake-languagetool-current-cand nil))

(defun flymake-languagetool--check-buffer ()
  "TODO: Document this."
  (when (bound-and-true-p flymake-mode)
    (flymake-start)))

(defun flymake-languagetool--ignore (ov id type)
  "Ignore LanguageTool ID at OV.
Depending on TYPE, either ignore Rule ID or Category ID."
  (let ((desc (map-elt (flymake-diagnostic-data
                        (overlay-get ov 'flymake-diagnostic))
                       'rule-desc)))
    (when (eq type 'Rule)
      (make-local-variable 'flymake-languagetool-disabled-rules)
      (add-to-list 'flymake-languagetool-disabled-rules id))
    (when (eq type 'Category)
      (make-local-variable 'flymake-languagetool-disabled-categories)
      (add-to-list 'flymake-languagetool-disabled-categories id))
    (flymake-languagetool--check-buffer)
    (message "%s %s: (%s) has been disabled" type id desc)
    (flymake-languagetool--clean-overlay)))

(defun flymake-languagetool--correct (ov choice)
  "Replace text in error at OV with CHOICE."
  (let ((start (overlay-start ov))
        (end (overlay-end ov)))
    (undo-boundary)
    (delete-overlay ov)
    (delete-region start end)
    (goto-char start)
    (insert choice)))

;; Lifted from jinx.el but will ensure users have a somewhat consistent
;; experience
(defun flymake-languagetool--correct-setup ()
  "Ensure that the minibuffer is setup for corrections."
  (let ((message-log-max nil)
        (inhibit-message t))
    (when (and (eq completing-read-function #'completing-read-default)
               (not (bound-and-true-p vertico-mode))
               (not (bound-and-true-p icomplete-mode)))
      (minibuffer-completion-help))))

;;
;;; Corrections

;;;###autoload
(defun flymake-languagetool-next (&optional n)
  "Go to Nth next flymake languagetool error."
  (interactive (list (or current-prefix-arg 1)))
  (let* ((ovs (if (> n 0)
                  (flymake-languagetool--ovs)
                (nreverse (flymake-languagetool--ovs))))
         (tail (seq-drop-while (lambda (ov) (if (> n 0)
                                                (<= (overlay-start ov) (point))
                                              (>= (overlay-start ov) (point))))
                               ovs))
         (chain (if flymake-wrap-around
                    (seq-concatenate 'list tail ovs)
                  tail))
         (target (nth (1- (abs n)) chain)))
    (goto-char (overlay-start target))))

;;;###autoload
(defun flymake-languagetool-previous (&optional n)
  "Go to Nth previous flymake languagetool error."
  (interactive (list (or current-prefix-arg 1)))
  (flymake-languagetool-next (- n)))

;;;###autoload
(defun flymake-languagetool-correct-at-point (&optional ol)
  "Correct `flymake-languagetool' diagnostic at point.
Use OL as diagnostic if non-nil."
  (interactive)
  (if-let* ((flymake-languagetool-current-cand
             (or ol (flymake-languagetool--ov-at-point))))
      (condition-case nil
          (when-let*
              ((ov flymake-languagetool-current-cand)
               (type (map-elt (flymake-diagnostic-data
                               (overlay-get ov 'flymake-diagnostic))
                              'type))
               (sugs (flymake-languagetool--suggestions))
               (prompt (flymake-diagnostic-text
                        (overlay-get ov 'flymake-diagnostic)))
               (id (map-elt (flymake-diagnostic-data
                             (overlay-get ov 'flymake-diagnostic))
                            'rule-id))
               (choice (minibuffer-with-setup-hook
                           #'flymake-languagetool--correct-setup
                         (completing-read
                          (format "Correction (%s): " prompt) sugs nil t nil nil
                          (car sugs)))))
            (pcase choice
              ("Ignore Rule" (flymake-languagetool--ignore ov id 'Rule))
              ("Ignore Category"
               (flymake-languagetool--ignore ov id 'Category))
              (_ (flymake-languagetool--correct ov choice))))
        (t (flymake-languagetool--clean-overlay)))
    (user-error "No correction at point")))

;;;###autoload
(defun flymake-languagetool-correct ()
  "Use `completing-read' to select and correct diagnostic."
  (interactive)
  (let* ((cands (flymake-languagetool--ovs 'format))
         (cand (if cands
                   (minibuffer-with-setup-hook
                       #'flymake-languagetool--correct-setup
                     (completing-read "Error: " cands nil t))
                 (user-error "No candidates")))
         (ov (map-elt cands cand)))
    (save-excursion
      (goto-char (overlay-start ov))
      (condition-case nil
          (funcall #'flymake-languagetool-correct-at-point ov)
        (quit (flymake-languagetool--clean-overlay))
        (t (flymake-languagetool--clean-overlay))))))

;;;###autoload
(defun flymake-languagetool-correct-dwim ()
  "DWIM function for correcting `flymake-languagetool' diagnostics."
  (interactive)
  (if-let* ((ov (flymake-languagetool--ov-at-point)))
      (funcall #'flymake-languagetool-correct-at-point ov)
    (funcall-interactively #'flymake-languagetool-correct)))

;;
;;; Entry

;;;###autoload
(defun flymake-languagetool-load ()
  "Convenience function to setup flymake-languagetool.
This adds the language-tool checker to the list of flymake diagnostic
functions."
  (add-hook 'flymake-diagnostic-functions #'flymake-languagetool--checker nil t))

;;;###autoload
(defun flymake-languagetool-maybe-load ()
  "Load backend if major-mode in `flymake-languagetool-active-modes'."
  (interactive)
  (when (memq major-mode flymake-languagetool-active-modes)
    (flymake-languagetool-load)))

(provide 'flymake-languagetool)
;;; flymake-languagetool.el ends here

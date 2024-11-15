;;; prettier-org.el --- Format source blocks in Org mode with prettier -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/prettier-org
;; Keywords: convenience, languages
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides functionality to format Org mode source blocks using
;; Prettier. It supports various languages and allows customization of
;; formatters and arguments.

;; Commands

;; M-x `prettier-org-format' (&optional argument)
;;      Format current src body at point with prettier if corresponding parser found.
;;      Parsers for src languages listed in `prettier-org-src-parsers-alist'.
;;      Common options listed in `prettier-org-args'.
;;      Alternatively if ARGUMENT is non-nil (interactively, with prefix argument),
;;      read prettier options in minibuffer.

;; M-x `prettier-org-format-all-src-blocks' (&optional file)
;;      Format all src block in FILE or current buffer.

;; Customization

;; `prettier-org-src-parsers-alist'
;;      Alist of org src languages and corresponding prettier parsers.

;; `prettier-org-args'
;;      List of args to send to prettier command excluding parser.
;;      Parser is specified in the variable `prettier-org-src-parsers-alist'.

;; `prettier-org-langs-formatters'
;;  An alist mapping language names to custom formatting functions.

;;; Code:


(require 'org)

(defcustom prettier-org-src-parsers-alist '(("js" . "typescript")
                                            ("typescript" . "typescript")
                                            ("javascript" . "typescript")
                                            ("ts" . "typescript")
                                            ("json" . "json"))
  "Alist mapping source block languages to their corresponding Prettier parsers.

An alist mapping Org mode source block languages to Prettier
parsers. Each key is a string representing the language used in
an Org mode source block, and the corresponding value is a
string representing the Prettier parser to use for that language."
  :type '(alist
          :key-type (string :tag "Language")
          :value-type (radio :tag "Prettier parser"
                       (const :tag "Flow" "flow")
                       (const :tag "Babel" "babel")
                       (const :tag "Babel-Flow" "babel-flow")
                       (const :tag "Babel-Ts" "babel-ts")
                       (const :tag "Typescript" "typescript")
                       (const :tag "Espree" "espree")
                       (const :tag "Meriyah" "meriyah")
                       (const :tag "Css" "css")
                       (const :tag "Less" "less")
                       (const :tag "Scss" "scss")
                       (const :tag "Json" "json")
                       (const :tag "Json5" "json5")
                       (const :tag "Json-Stringify" "json-stringify")
                       (const :tag "Graphql" "graphql")
                       (const :tag "Markdown" "markdown")
                       (const :tag "Mdx" "mdx")
                       (const :tag "Vue" "vue")
                       (const :tag "Yaml" "yaml")
                       (const :tag "Glimmer" "glimmer")
                       (const :tag "Html" "html")
                       (const :tag "Angular" "angular")
                       (const :tag "Lwc" "lwc")
                       (string :tag "Other parser")))
  :group 'prettier-org)

(defcustom prettier-org-langs-formatters nil
  "Alist mapping programming languages to custom formatting functions.

An alist mapping language names to custom formatting functions.

Each key should be a string representing the language name, and
each value should be a function that takes a single argument,
the code to be formatted, and returns the formatted code as a
string.

Example usage:

\\(setq prettier-org-langs-formatters
      \\='((\"python\" . my-python-formatter)
          (\"emacs-lisp\" . prettier-elisp-string)
          (\"elisp\" . prettier-elisp-string)))

In this example, `my-python-formatter', `prettier-elisp-string'
are user-defined functions that take a string of code and return
a formatted string. These functions will be used to format
Python and JavaScript code blocks, respectively, in Org mode."
  :type '(alist
          :key-type (string :tag "Language")
          :value-type (function :tag "Custom function"))
  :group 'prettier-org)

(defcustom prettier-org-args '()
  "List of args to send to prettier command excluding parser.
Parser is specified in the variable `prettier-org-src-parsers-alist'."
  :type '(repeat string)
  :group 'prettier-org)

(defun prettier-org-get-prettier-params ()
  "If point is inside body of src block return list - (LANGUAGE BEGINNING END)."
  (save-excursion
    (save-restriction
      (widen)
      (let ((case-fold-search t))
        (when (re-search-forward "#\\+\\(begin\\|end\\)_src\\($\\|[\s\f\t\n\r\v]\\)" nil t 1)
          (when-let* ((word (match-string-no-properties 1))
                     (end (match-beginning 0)))
            (setq word (downcase word))
            (when (string= word "end")
              (when (re-search-backward "^\\([ \t]*\\)#\\+begin_src[ \t]+\\([^ \f\t\n\r\v]+\\)[ \t]*" nil t 1)
                (let ((lang (match-string-no-properties 2)))
                  (forward-line 1)
                  (list lang (point) end))))))))))

(defun prettier-org-file-parent (path)
  "Return the parent directory to PATH without slash."
  (let ((parent (file-name-directory
                 (directory-file-name
                  (expand-file-name path default-directory)))))
    (when (and (file-exists-p path)
               (file-exists-p parent)
               (not (equal
                     (file-truename (directory-file-name
                                     (expand-file-name path)))
                     (file-truename (directory-file-name
                                     (expand-file-name parent))))))
      (if (file-name-absolute-p path)
          (directory-file-name parent)
        (file-relative-name parent)))))

(defvar-local prettier-org-exec-path nil)
(defun prettier-org-find-exec ()
  "Return prettier executable, either from node_modules or globally."
  (setq prettier-org-exec-path
        (or prettier-org-exec-path
            (let ((dir default-directory)
                  (node-modules)
                  (found))
              (while (setq node-modules
                           (unless found
                             (setq dir (locate-dominating-file
                                        dir
                                        "node_modules"))))
                (setq dir (prettier-org-file-parent dir))
                (let ((file (expand-file-name "node_modules/.bin/prettier"
                                              node-modules)))
                  (setq found (when (and (file-exists-p file)
                                         (file-executable-p file))
                                file))))
              (or found (executable-find "prettier"))))))

(defun prettier-org-list-parsers (&optional prettier-exec)
  "Return list of prettier parsers from output of PRETTIER-EXEC help command.
If PRETTIER-EXEC is nil, search for local or global prettier executable."
  (when-let* ((prettier (or prettier-exec (prettier-org-find-exec))))
    (with-temp-buffer
      (shell-command (concat prettier " --help") (current-buffer))
      (when-let* ((parsers
                  (when (re-search-forward
                         "--parser[\s\t\n]+[<]\\([^>]+\\)>" nil t 1)
                    (match-string-no-properties 1))))
        (split-string parsers "|" t)))))

(defun prettier-org-format-string (string &rest options)
  "Apply prettier on STRING with OPTIONS.
Return list of two elements: status (t or nil) and string with result."
  (when-let* ((prettier-cmd (prettier-org-find-exec)))
    (with-temp-buffer
      (insert string)
      (list
       (eq 0
           (apply #'call-process-region
                  (append
                   (list (point-min)
                         (point-max)
                         prettier-cmd
                         t
                         t
                         nil)
                   (flatten-list options))))
       (buffer-string)))))

(defun prettier-org-commment-noweb (lang code)
  "Wrap noweb references in CODE in comments according to LANG.
Return a cons of CODE with commented noweb references and alist
of replacements and original values"
  (when-let* ((mode (org-src-get-lang-mode lang)))
    (with-temp-buffer
      (insert code)
      (goto-char (point-min))
      (delay-mode-hooks
        (funcall mode)
        (if-let* ((repls (prettier-org-commment-noweb-0)))
            (cons (buffer-string) repls)
          (cons code '()))))))

(defun prettier-org-commment-noweb-0 ()
  "Wrap noweb referneces in CODE in comments according to LANG.
Return alist of replacements and original values."
  (let ((replacements)
        (re (org-babel-noweb-wrap))
        (inhibit-read-only t))
    (while (re-search-forward re nil t 1)
      (let ((beg (match-beginning 0))
            (end (match-end 0))
            (ref (match-string-no-properties 0))
            (rep))
        (comment-region-default beg end)
        (setq rep (string-trim (buffer-substring-no-properties beg (point))))
        (push (cons rep ref) replacements)))
    replacements))

(defun prettier-org-replace-matches (replacements code)
  "Search and replace occurences in CODE according to REPLACEMENTS.
REPLACEMENTS should be alist of strings to search for and associated value to
replace with."
  (with-temp-buffer
    (insert code)
    (let ((curr))
      (while (setq curr (pop replacements))
        (when (re-search-backward (regexp-quote (car curr)) nil t 1)
          (save-excursion (replace-match (cdr curr))))))
    (buffer-string)))

(defun prettier-org-format-region (lang beg end &rest options)
  "Format region between BEG and END with LANG and OPTIONS."
  (setq options (flatten-list options))
  (let ((code (buffer-substring-no-properties beg end))
        (parser (or (car (seq-drop (member "--parser" options) 1)))))
    (let ((prefix
           (when (and (string-prefix-p "{" (string-trim code))
                      (null (member parser '("json" "json5"))))
             "let temp_var = "))
          (cell))
      (setq cell (prettier-org-commment-noweb
                  lang
                  (if prefix
                      (concat prefix code)
                    code)))
      (let* ((result
              (prettier-org-format-string
               (car cell)
               options))
             (str
              (when (car result)
                (if prefix
                    (substring
                     (cadr result)
                     (length prefix))
                  (cadr result))))
             (final
              (when str
                (prettier-org-replace-matches
                 (cdr cell)
                 str))))
        (if (car result)
            (replace-region-contents beg end (lambda ()
                                               final))
          (message (cadr result)))))))

(defun prettier-org-custom-formatter (lang beg end)
  "Format region between BEG and END with custom formatter for LANG."
  (when-let* ((custom-formatter (cdr (assoc lang
                                            prettier-org-langs-formatters)))
              (code (buffer-substring-no-properties
                     beg
                     end))
              (cell (prettier-org-commment-noweb
                     lang
                     code))
              (formatted (funcall custom-formatter (car cell))))
    (setq formatted (prettier-org-replace-matches
                     (cdr cell)
                     formatted))
    (unless (string= code formatted)
      (replace-region-contents beg end
                               (lambda () formatted)))))

(defun prettier-org-find-lang-by-mode ()
  "Find current major-mode in `org-src-lang-modes'."
  (let ((mode (symbol-name major-mode)))
    (seq-find (lambda (it)
                (string= mode
                         (format "%s-mode" (cdr it))))
              org-src-lang-modes)))

(defun prettier-org-next-read-only-property-change ()
  "Jump to the position of next read-only property change.
Return the position of point if found, or nil."
  (when-let* ((beg (next-single-property-change (point) 'read-only)))
    (goto-char beg)
    beg))

(defun prettier-org-buffer-format-region (beg end)
  "Run PRETTIER-FN with ARGS on region between BEG and END."
  (let* ((buff (current-buffer))
         (content (buffer-substring-no-properties
                   beg
                   end))
         (mode major-mode)
         (formatted (with-temp-buffer
                      (insert content)
                      (goto-char (point-min))
                      (let ((reps (prettier-org-commment-noweb-0)))
                        (funcall mode)
                        (goto-char (point-min))
                        (save-excursion
                          (re-search-forward "[.]" nil t 1)
                          (run-hooks 'before-save-hook))
                        (prettier-org-replace-matches
                         reps
                         (buffer-substring-no-properties (point-min)
                                                         (point-max)))))))
    (when (and formatted
               (not (string= content formatted)))
      (with-current-buffer buff
        (let ((inhibit-read-only t))
          (replace-region-contents beg end (lambda () formatted))
          formatted)))))

(defun prettier-org-format-non-readonly-regions ()
  "Run `before-save-hook' in temporarly buffer with commented noweb references.
Also skip read-only properties.
Supposed to use in `org-src-mode-hook'."
  (cond ((save-excursion
           (goto-char (point-min))
           (prettier-org-next-read-only-property-change))
         (save-excursion
           (let* ((fn (lambda ()
                        (let* ((start (point))
                               (end
                                (or
                                 (prettier-org-next-read-only-property-change)
                                 (point-max))))
                          (unless (or (bobp)
                                      (eobp))
                            (when (and (get-text-property (1- (point))
                                                          'read-only)
                                       (not (get-text-property (1+ (point))
                                                               'read-only)))
                              (forward-char 1)
                              (setq end (point))))
                          (prettier-org-buffer-format-region start end)))))
             (goto-char (point-min))
             (unless (get-text-property (point) 'read-only)
               (funcall fn))
             (while
                 (when (get-text-property (point) 'read-only)
                   (prettier-org-next-read-only-property-change))
               (funcall fn)))))
        (t
         (let ((reps
                (save-excursion
                  (goto-char (point-min))
                  (prettier-org-commment-noweb-0))))
           (run-hooks 'before-save-hook)
           (prettier-org-replace-matches
            reps
            (buffer-substring-no-properties (point-min)
                                            (point-max)))))))

(defun prettier-org-format (&optional argument)
  "Format src body at point with prettier if corresponding parser found.
Parsers for src languages listed in `prettier-org-src-parsers-alist'.
Common options listed in `prettier-org-args'.
Alternatively if ARGUMENT is non-nil (interactively, with prefix argument),
read prettier options in minibuffer."
  (interactive "P")
  (when-let* ((params (prettier-org-get-prettier-params))
              (lang (car params)))
    (if-let* ((custom-formatter
               (cdr (assoc lang prettier-org-langs-formatters)))
              (code (buffer-substring-no-properties
                     (nth 1 params)
                     (nth 2 params)))
              (cell (prettier-org-commment-noweb
                     lang
                     code))
              (formatted (funcall custom-formatter
                                  (car cell))))
        (progn
          (setq formatted (prettier-org-replace-matches
                           (cdr cell)
                           formatted))
          (unless (string= code formatted)
            (replace-region-contents (nth 1 params)
                                     (nth 2 params)
                                     (lambda ()
                                       formatted))))
      (let* ((parser (cdr (assoc lang prettier-org-src-parsers-alist)))
             (options
              (if argument
                  (split-string
                   (read-string
                    "Options:\s"
                    (string-join
                     (delete nil
                             (append
                              prettier-org-args
                              (list
                               "--parser"
                               (or parser
                                   (completing-read
                                    "--parser\s"
                                    (prettier-org-list-parsers))))))
                     "\s"))
                   nil t)
                (when parser
                  (append (list "--parser" parser)
                          prettier-org-args)))))
        (when options
          (prettier-org-format-region lang (nth 1 params)
                                      (nth 2 params)
                                      options))))))

(defun prettier-org-format-all-src-blocks (&optional file)
  "Format all src blocks in the FILE or current buffer."
  (interactive)
  (org-babel-map-src-blocks file
    (if (cdr (assoc lang prettier-org-langs-formatters))
        (prettier-org-custom-formatter lang beg-body end-body)
      (when-let* ((parser (cdr (assoc lang prettier-org-src-parsers-alist))))
        (unless (or (null body)
                    (string-empty-p (string-trim body)))
          (prettier-org-format-region lang beg-body end-body "--parser" parser
                                      prettier-org-args))))))

(defun prettier-org-src-run-save-hooks ()
  "Run `prettier-org-format-non-readonly-regions' when saving an Org source block."
  (when (eq this-command 'org-edit-src-save)
    (prettier-org-format-non-readonly-regions)))

;;;###autoload
(define-minor-mode prettier-org-edit-src-mode
  "Enable or disable automatic formatting of Org source blocks before saving.

Enable or disable automatic formatting of non-read-only regions in Org source
code blocks before saving. When enabled, hooks are added to format the code,
ensuring that noweb references are commented and read-only regions are
preserved.

Supposed to use in `org-src-mode-hook':

\=(add-hook \\='org-src-mode-hook #\\='prettier-org-edit-src-mode)."
  :global nil
  (if prettier-org-edit-src-mode
      (add-hook 'pre-command-hook #'prettier-org-src-run-save-hooks nil t)
    (remove-hook 'pre-command-hook #'prettier-org-src-run-save-hooks t)))

;;;###autoload
(define-minor-mode prettier-org-mode
  "Runs prettier on save when point is inside org src block."
  :global nil
  (if prettier-org-mode
      (add-hook 'before-save-hook #'prettier-org-format nil 'local)
    (remove-hook 'before-save-hook #'prettier-org-format 'local)))

(provide 'prettier-org)
;;; prettier-org.el ends here
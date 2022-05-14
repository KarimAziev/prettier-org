;;; prettier-org.el --- Configure prettier -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/prettier-org
;; Keywords: convenience, languages
;; Version: 0.1.1
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

;; This file configures operations with prettier

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


;;; Code:


(require 'org)

(defcustom prettier-org-src-parsers-alist '(("js" . "typescript")
                                            ("typescript" . "typescript")
                                            ("javascript" . "typescript")
                                            ("ts" . "typescript"))
  "Alist of org src languages and corresponding prettier parsers."
  :type '(alist :key-type (string :tag "Language")
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
                                   (string :tag "Other")))
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
          (when-let ((word (match-string-no-properties 1))
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
  (when-let ((prettier (or prettier-exec (prettier-org-find-exec))))
    (with-temp-buffer
      (shell-command (concat prettier " --help") (current-buffer))
      (when-let ((parsers
                  (when (re-search-forward
                         "--parser[\s\t\n]+[<]\\([^>]+\\)>" nil t 1)
                    (match-string-no-properties 1))))
        (split-string parsers "|" t)))))

(defun prettier-org-format-string (string &rest options)
  "Apply prettier on STRING with OPTIONS.
Return list of two elements: status (t or nil) and string with result."
  (when-let ((prettier-cmd (prettier-org-find-exec)))
    (with-temp-buffer
      (insert string)
      (list
       (eq 0
           (apply 'call-process-region
                  (append
                   (list (point-min)
                         (point-max)
                         prettier-cmd
                         t
                         t
                         nil)
                   (flatten-list options))))
       (buffer-string)))))

(defun prettier-org-commment-noweb (code)
  "Wrap noweb referneces in CODE in comments."
  (let ((re (org-babel-noweb-wrap)))
    (with-temp-buffer
      (insert code)
      (while (re-search-backward re nil t 1)
        (let ((beg (match-beginning 0))
              (end (match-end 0)))
          (replace-region-contents beg end
                                   (lambda () (concat
                                          "/*"
                                          (buffer-substring-no-properties
                                           beg end)
                                          "*/")))))
      (buffer-string))))

(defun prettier-org-uncommment-noweb (code)
  "Uncommeent noweb referneces in CODE."
  (let ((re (org-babel-noweb-wrap)))
    (with-temp-buffer
      (insert code)
      (while (re-search-backward re nil t 1)
        (let ((ref (match-string-no-properties 0))
              (beg (match-beginning 0))
              (end (match-end 0)))
          (let ((left (buffer-substring-no-properties (- beg 2) beg))
                (right (buffer-substring-no-properties end (+ end 2))))
            (when (and (string= left "/*")
                       (string= right "*/"))
              (replace-region-contents (- beg 2) (+ end 2) (lambda () ref))))))
      (buffer-string))))

(defun prettier-org-format-region (beg end &rest options)
  "Format region between BEG and END with OPTIONS."
  (setq options (flatten-list options))
  (let ((code (buffer-substring-no-properties beg end))
        (parser (or (car (seq-drop (member "--parser" options) 1)))))
    (let ((prefix (when (and (string-prefix-p "{" (string-trim code))
                             (null (member parser '("json" "json5"))))
                    "let temp_var = ")))
      (setq code (prettier-org-commment-noweb
                  (if prefix
                      (concat prefix code)
                    code)))
      (let ((result
             (prettier-org-format-string
              code
              options)))
        (if (car result)
            (replace-region-contents beg end (lambda ()
                                               (prettier-org-uncommment-noweb
                                                (if prefix
                                                    (substring
                                                     (cadr result)
                                                     (length prefix))
                                                  (cadr result)))))
          (message (cadr result)))))))

(defun prettier-org-format (&optional argument)
  "Format src body at point with prettier if corresponding parser found.
Parsers for src languages listed in `prettier-org-src-parsers-alist'.
Common options listed in `prettier-org-args'.
Alternatively if ARGUMENT is non-nil (interactively, with prefix argument),
read prettier options in minibuffer."
  (interactive "P")
  (when-let ((params (prettier-org-get-prettier-params)))
    (let* ((parser (cdr (assoc (car params) prettier-org-src-parsers-alist)))
           (options
            (if argument
                (split-string (read-string
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
        (prettier-org-format-region (nth 1 params) (nth 2 params) options)))))

(defun prettier-org-format-all-src-blocks (&optional file)
  "Format all src blocks in the FILE or current buffer."
  (interactive)
  (org-babel-map-src-blocks file
    (when-let ((parser (cdr (assoc lang prettier-org-src-parsers-alist))))
      (unless (or (null body) (string-empty-p (string-trim body)))
        (prettier-org-format-region beg-body end-body "--parser" parser
                                    prettier-org-args)))))

;;;###autoload
(define-minor-mode prettier-org-mode
  "Runs prettier on save when point is inside org src block."
  :global nil
  (if prettier-org-mode
      (add-hook 'before-save-hook 'prettier-org-format nil 'local)
    (remove-hook 'before-save-hook 'prettier-org-format 'local)))

(provide 'prettier-org)
;;; prettier-org.el ends here
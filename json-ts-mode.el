;;; json-ts-mode.el --- Tree-sitter mode for JSON and JSONC  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 konomanoasa
;;
;; Author: konomanoasa <238482287+konomanoasa@users.noreply.github.com>
;; Maintainer: konomanoasa <238482287+konomanoasa@users.noreply.github.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1"))
;; Keywords: languages
;; URL: https://github.com/konomanoasa/json-ts-mode
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:
;;
;; Tree-sitter major mode for JSON and JSONC.

;;; Code:

(require 'editorconfig)
(require 'elec-pair)
(require 'newcomment)
(require 'paren)
(require 'treesit)

(defgroup json-ts nil
  "Tree-sitter mode for JSON and JSONC."
  :group 'languages)

;;;; Grammar

(defconst json-ts-mode--grammar-sources
  '((json "https://github.com/konomanoasa/tree-sitter-json"
          :revision "v0.2.2")
    (jsonc "https://github.com/konomanoasa/tree-sitter-json"
           :revision "v0.2.2" :source-dir "jsonc/src")
    (jsonc_trailing "https://github.com/konomanoasa/tree-sitter-json"
                    :revision "v0.2.2" :source-dir "jsonc_trailing/src"))
  "Tree-sitter grammar sources for JSON and JSONC.")

(defconst json-ts-mode--file-name-languages
  '(("tsconfig.json" . jsonc_trailing)
    ("jsconfig.json" . jsonc_trailing))
  "Default Tree-sitter languages for known file basenames.")

(defun json-ts-mode--ensure-grammar (language)
  "Ensure that the grammar for LANGUAGE is installed."
  (let ((treesit-language-source-alist
         (if (assq language treesit-language-source-alist)
             treesit-language-source-alist
           (cons (assq language json-ts-mode--grammar-sources)
                 treesit-language-source-alist))))
    (or (treesit-ensure-installed language)
        (user-error "Tree-sitter grammar `%s' is unavailable" language))))

(defun json-ts-mode--language ()
  "Return the Tree-sitter language selected for the current buffer."
  (let ((dialect (when buffer-file-name
                   (gethash 'json_dialect
                            (editorconfig-call-get-properties-function
                             buffer-file-name)))))
    (pcase dialect
      ('nil (if buffer-file-name
                (or (cdr (assoc (file-name-nondirectory buffer-file-name)
                                json-ts-mode--file-name-languages))
                    (if (equal (file-name-extension buffer-file-name) "jsonc")
                        'jsonc 'json))
              'json))
      ("json" 'json)
      ("jsonc" 'jsonc)
      ("jsonc_trailing" 'jsonc_trailing)
      (_ (user-error "Invalid EditorConfig json_dialect: %S" dialect)))))

;;;; Incremental Updates

(defconst json-ts-mode-update--containers
  '("value" "object" "array")
  "Container nodes traversed to find an edited source unit.")

(defvar-local json-ts-mode-update--state nil
  "Bounds and recovery state captured before a buffer edit.")

(defvar-local json-ts-mode-update--pending-range nil
  "Edited owner range awaiting syntax propertization.")

(defun json-ts-mode-update--bounds (beg end)
  "Return enclosing source-unit bounds around BEG and END."
  (let ((start beg)
        (finish end)
        (root (treesit-parser-root-node treesit-primary-parser)))
    (dolist (position (list (max (point-min) (1- beg))
                            (min (point-max) end)))
      (let ((node (treesit-node-at position treesit-primary-parser))
            owner)
        (while (and node (not (treesit-node-eq node root)))
          (unless (member (treesit-node-type node)
                          json-ts-mode-update--containers)
            (setq owner node))
          (setq node (treesit-node-parent node)))
        (when owner
          (setq start (min start (treesit-node-start owner))
                finish (max finish (treesit-node-end owner))))))
    (cons start finish)))

(defun json-ts-mode-update--before-change (beg end)
  "Save owner bounds and old-tree recovery state before changing BEG to END."
  (when treesit-primary-parser
    (save-restriction
      (widen)
      (setq json-ts-mode-update--state
            (cons (json-ts-mode-update--bounds beg end)
                  (treesit-node-check
                   (treesit-parser-root-node treesit-primary-parser)
                   'has-error))))))

(defun json-ts-mode-update--reset-parser ()
  "Replace the primary parser without reusing its old tree."
  (let* ((parser treesit-primary-parser)
         (language (treesit-parser-language parser))
         (tag (treesit-parser-tag parser))
         (ranges (treesit-parser-included-ranges parser))
         (notifiers (treesit-parser-notifiers parser))
         (level (treesit-parser-embed-level parser)))
    (treesit-parser-delete parser)
    (setq treesit-primary-parser (treesit-parser-create language nil nil tag))
    (treesit-parser-set-included-ranges treesit-primary-parser ranges)
    (treesit-parser-set-embed-level treesit-primary-parser level)
    (dolist (notifier notifiers)
      (treesit-parser-add-notifier treesit-primary-parser notifier))))

(defun json-ts-mode-update--after-change (beg end old-length)
  "Refresh edited owners and avoid stale trees after BEG, END and OLD-LENGTH."
  (when treesit-primary-parser
    (let ((narrowed (buffer-narrowed-p)))
      (save-restriction
        (widen)
        (let* ((state json-ts-mode-update--state)
               (reset (or (cdr state)
                          (and (= beg 1) (> old-length 0)
                               (eq (char-after 1) #xfeff)))))
          (setq json-ts-mode-update--state nil)
          (when reset (json-ts-mode-update--reset-parser))
          (let* ((bounds (if reset (cons (point-min) (point-max))
                           (json-ts-mode-update--bounds beg end)))
                 (start (min (car bounds) (or (caar state) beg)))
                 (finish (min (point-max)
                              (max (cdr bounds)
                                   (+ (or (cdar state) (+ beg old-length))
                                      (- end beg old-length)))))
                 (pending json-ts-mode-update--pending-range)
                 (pending-end (cdr pending)))
            (when (and pending-end (>= pending-end beg))
              (setq pending-end (max end (+ pending-end (- end beg old-length)))))
            (setq json-ts-mode-update--pending-range
                  (cons (min start (or (car pending) start))
                        (min (point-max) (max finish (or pending-end finish)))))
            (syntax-ppss-flush-cache start)
            (font-lock-flush start finish)
            (when narrowed (syntax-propertize finish))))))))

(defun json-ts-mode-update--extend-region (start end)
  "Include edited owners when propertizing START to END."
  (when json-ts-mode-update--pending-range
    (let ((bounds json-ts-mode-update--pending-range))
      (setq json-ts-mode-update--pending-range nil)
      (cons (max (point-min) (min start (car bounds)))
            (min (point-max) (max end (cdr bounds)))))))

(defun json-ts-mode-update--setup ()
  "Configure incremental updates for the current buffer."
  (add-hook 'before-change-functions #'json-ts-mode-update--before-change nil t)
  (add-hook 'after-change-functions #'json-ts-mode-update--after-change nil t)
  (add-hook 'syntax-propertize-extend-region-functions
            #'json-ts-mode-update--extend-region nil t))

;;;; Syntax

(defvar json-ts-mode-syntax--text-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    (dolist (character '(?\" ?\' ?` ?\\ ?/ ?* ?\( ?\) ?\[ ?\] ?{ ?}))
      (modify-syntax-entry character "." table))
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\r "> b" table)
    table)
  "Syntax table for text without a CST syntax classification.")

(defvar json-ts-mode-syntax-table
  (let ((table (copy-syntax-table json-ts-mode-syntax--text-table)))
    (dolist (entry '((?\[ . "(]") (?\] . ")[") (?{ . "(}") (?} . "){")))
      (modify-syntax-entry (car entry) (cdr entry) table))
    table)
  "Syntax table for `json-ts-mode'.")

(defvar json-ts-mode-syntax--comment-table
  (let ((table (copy-syntax-table json-ts-mode-syntax--text-table)))
    (modify-syntax-entry ?\n "." table)
    (modify-syntax-entry ?\r "." table)
    table)
  "Syntax table for the body of a multi-line comment.")

;;;;; Syntax Queries

(defconst json-ts-mode-syntax--queries
  (mapcar (lambda (source)
            (let ((language (car source)))
              (cons language
                    (treesit-query-compile
                     language
                     (append
                      '((string) @string
                        (array "[" @open) (array "]" @close)
                        (object "{" @open) (object "}" @close))
                      (unless (eq language 'json)
                        '((single_line_comment) @line-comment
                          (multi_line_comment) @block-comment)))))))
          json-ts-mode--grammar-sources)
  "Compiled syntax queries by language for JSON and JSONC.")

;;;;; Propertization

(defun json-ts-mode-syntax--propertize (start end)
  "Apply syntax properties between START and END."
  (let ((accessible-start (point-min)))
    (save-restriction
      (widen)
      (when (and (= start accessible-start)
                 (> accessible-start (point-min)))
        (setq start (point-min))
        (syntax-ppss-flush-cache start))
      (put-text-property start end 'syntax-table json-ts-mode-syntax--text-table)
      (dolist (capture (treesit-query-capture
                        (treesit-parser-root-node treesit-primary-parser)
                        (alist-get (treesit-parser-language treesit-primary-parser)
                                   json-ts-mode-syntax--queries)
                        start end))
        (let* ((name (car capture))
               (node (cdr capture))
               (begin (treesit-node-start node))
               (finish (treesit-node-end node)))
          (unless (or (= begin finish) (treesit-node-check node 'missing))
            (pcase name
              ((or 'string 'line-comment 'block-comment)
               (put-text-property begin finish 'syntax-table
                                  json-ts-mode-syntax--text-table)
               (let ((opening (treesit-node-child node 0))
                     (closing (treesit-node-child node -1)))
                 (pcase name
                   ('string
                    (when (and (not (treesit-node-check node 'has-error))
                               (equal (treesit-node-type opening) "\"")
                               (equal (treesit-node-type closing) "\"")
                               (< (treesit-node-start opening)
                                  (treesit-node-start closing)))
                      (put-text-property begin (1+ begin) 'syntax-table
                                         (string-to-syntax "|"))
                      (put-text-property (1- finish) finish 'syntax-table
                                         (string-to-syntax "|"))))
                   ('line-comment
                    (when (equal (treesit-node-type opening) "//")
                      (put-text-property begin (1+ begin) 'syntax-table
                                         (string-to-syntax "< b"))))
                   ('block-comment
                    (when (equal (treesit-node-type opening) "/*")
                      (put-text-property begin finish 'syntax-table
                                         json-ts-mode-syntax--comment-table)
                      (put-text-property begin (1+ begin) 'syntax-table
                                         (string-to-syntax "<")))
                    (when (and (equal (treesit-node-type closing) "*/")
                               (not (treesit-node-check closing 'missing)))
                      (put-text-property (1- finish) finish 'syntax-table
                                         (string-to-syntax ">")))))))
              ((or 'open 'close)
               (put-text-property
                begin finish 'syntax-table
                (string-to-syntax
                 (pcase (treesit-node-type node)
                   ("[" "(]") ("]" ")[") ("{" "(}") ("}" "){"))))))))))))

;;;;; Matching Delimiters

(defun json-ts-mode-syntax--show-paren-data ()
  "Return matching delimiter ranges from their CST owner."
  (save-restriction
    (widen)
    (let ((position (point)))
      (catch 'match
        (dolist (capture (treesit-query-capture
                          (treesit-parser-root-node treesit-primary-parser)
                          (alist-get (treesit-parser-language treesit-primary-parser)
                                     json-ts-mode-syntax--queries)
                          (max (point-min) (1- position))
                          (min (point-max) (1+ position))))
          (let* ((name (car capture))
                 (node (cdr capture))
                 (owner (treesit-node-parent node))
                 (opening (treesit-node-child owner 0))
                 (closing (treesit-node-child owner -1)))
            (when (and (or (and (eq name 'open)
                                (= position (treesit-node-start node)))
                           (and (eq name 'close)
                                (= position (treesit-node-end node))))
                       (not (treesit-node-check opening 'missing))
                       (not (treesit-node-check closing 'missing))
                       (member (list (treesit-node-type opening)
                                     (treesit-node-type closing))
                               '(("[" "]") ("{" "}"))))
              (let ((other (if (eq name 'open) closing opening)))
                (throw 'match (list (treesit-node-start node)
                                    (treesit-node-end node)
                                    (treesit-node-start other)
                                    (treesit-node-end other)))))))))))

;;;;; Setup

(defun json-ts-mode-syntax--setup ()
  "Configure syntax handling for the current buffer."
  (setq-local syntax-propertize-function #'json-ts-mode-syntax--propertize)
  (add-hook 'syntax-propertize-extend-region-functions
            #'syntax-propertize-wholelines nil t)
  (setq-local show-paren-data-function #'json-ts-mode-syntax--show-paren-data))

;;;; Comment Commands

(defun json-ts-mode-comment--unsupported ()
  "Report that JSON does not support comments."
  (interactive)
  (user-error "JSON does not support comments"))

(defun json-ts-mode-comment--setup ()
  "Configure comment commands for the current buffer."
  (if (eq (treesit-parser-language treesit-primary-parser) 'json)
      (let ((map (make-sparse-keymap)))
        (define-key map [remap comment-dwim] #'json-ts-mode-comment--unsupported)
        (define-key map [remap comment-line] #'json-ts-mode-comment--unsupported)
        (use-local-map (make-composed-keymap map (current-local-map))))
    (setq-local comment-start "// ")
    (setq-local comment-end "")
    (setq-local comment-start-skip "\\(?://\\|/\\*\\)[[:blank:]]*")
    (setq-local comment-end-skip "[[:blank:]]*\\(?:\\*/\\|$\\)")
    (setq-local comment-use-syntax t)))

;;;; Electric Pair

(defun json-ts-mode-electric-pair--newline-context-p ()
  "Return non-nil between adjacent delimiters of an array or object."
  (save-restriction
    (widen)
    (when (and (eq (char-before) ?\n)
               (>= (- (point) 2) (point-min))
               (< (point) (point-max)))
      (let* ((opening (treesit-node-at (- (point) 2) treesit-primary-parser))
             (closing (treesit-node-at (point) treesit-primary-parser))
             (owner (treesit-node-parent opening)))
        (and (= (treesit-node-start opening) (- (point) 2))
             (= (treesit-node-end opening) (1- (point)))
             (= (treesit-node-start closing) (point))
             (= (treesit-node-end closing) (1+ (point)))
             (treesit-node-eq owner (treesit-node-parent closing))
             (member (list (treesit-node-type owner)
                           (treesit-node-type opening)
                           (treesit-node-type closing))
                     '(("array" "[" "]") ("object" "{" "}"))))))))

(defun json-ts-mode-electric-pair--setup ()
  "Configure electric pairing for the current buffer."
  (let ((pairs '((?\[ . ?\]) (?{ . ?}) (?\" . ?\")))
        (table (copy-syntax-table (syntax-table))))
    (setq-local electric-pair-pairs (copy-sequence electric-pair-pairs))
    (dolist (pair pairs)
      (unless (assq (car pair) electric-pair-pairs)
        (setq-local electric-pair-pairs (append electric-pair-pairs (list pair))))
      (unless (eq (cdr (assq (car pair) electric-pair-pairs)) (cdr pair))
        (modify-syntax-entry (car pair) "." table)))
    (set-syntax-table table))
  (let ((setting electric-pair-open-newline-between-pairs))
    (setq-local electric-pair-open-newline-between-pairs
                (lambda ()
                  (and (if (functionp setting) (funcall setting) setting)
                       (json-ts-mode-electric-pair--newline-context-p))))))

;;;; Font Lock

;;;;; Features

(defconst json-ts-mode-font-lock--feature-list
  '((comment constant)
    (property string)
    (number escape)
    (delimiter punctuation bracket))
  "Font-lock features by decoration level.")

;;;;; Settings

(defun json-ts-mode-font-lock--settings (language)
  "Return font-lock settings for LANGUAGE."
  (treesit-font-lock-rules
   :default-language language

   :feature 'comment
   (unless (eq language 'json)
     '((single_line_comment "//" @font-lock-comment-face)
       (multi_line_comment ["/*" "*/"] @font-lock-comment-face)
       (comment_text) @font-lock-comment-face))

   :feature 'constant
   '((value ["true" "false" "null"] @font-lock-constant-face))

   :feature 'string
   '((value (string (char (unescaped) @font-lock-string-face)))
     (string "\"" @font-lock-string-face))

   :feature 'property
   '((member name: (string (char (unescaped) @font-lock-property-name-face))))

   :feature 'number
   '([(digit1_9) (digit) (e)] @font-lock-number-face
     (int "0" @font-lock-number-face)
     (number "-" @font-lock-number-face)
     (frac "." @font-lock-number-face)
     (exp ["+" "-"] @font-lock-number-face))

   :feature 'escape
   '((char ["\\" "\"" "/" "b" "f" "n" "r" "t" "u"] @font-lock-escape-face)
     (invalid_escape ["\\" "u"] @font-lock-escape-face)
     (hexdig) @font-lock-escape-face)

   :feature 'delimiter
   '((member ":" @font-lock-punctuation-face))

   :feature 'punctuation
   '((object "," @font-lock-punctuation-face)
     (array "," @font-lock-punctuation-face)
     (unexpected_separator "," @font-lock-punctuation-face))

   :feature 'bracket
   '((object ["{" "}"] @font-lock-bracket-face)
     (array ["[" "]"] @font-lock-bracket-face))))

;;;;; Setup

(defun json-ts-mode-font-lock--setup ()
  "Configure font lock for the current buffer."
  (setq-local treesit-font-lock-feature-list json-ts-mode-font-lock--feature-list)
  (setq-local treesit-font-lock-settings
              (json-ts-mode-font-lock--settings
               (treesit-parser-language treesit-primary-parser))))

;;;; Navigation

(defun json-ts-mode-navigation--sexp-p (node)
  "Return non-nil if NODE is a representative editing unit."
  (let ((type (treesit-node-type node))
        (parent (treesit-node-parent node)))
    (and (< (treesit-node-start node) (treesit-node-end node))
         (or (equal type "member")
             (and (equal type "value")
                  (member (treesit-node-type parent) '("json_text" "member" "array")))
             (and (equal type "string")
                  (treesit-node-eq node (treesit-node-child-by-field-name parent "name")))))))

(defun json-ts-mode-navigation--defun-p (node)
  "Return non-nil if NODE is an item of the top-level container."
  (let* ((parent (treesit-node-parent node))
         (value (treesit-node-parent parent)))
    (and (< (treesit-node-start node) (treesit-node-end node))
         (member (list (treesit-node-type node) (treesit-node-type parent))
                 '(("member" "object") ("value" "array")))
         (equal (treesit-node-type value) "value")
         (equal (treesit-node-type (treesit-node-parent value)) "json_text"))))

(defconst json-ts-mode-navigation--settings
  (mapcar (lambda (source)
            (cons (car source)
                  '((sexp json-ts-mode-navigation--sexp-p)
                    (defun json-ts-mode-navigation--defun-p))))
          json-ts-mode--grammar-sources)
  "Tree-sitter thing definitions for JSON and JSONC.")

(defun json-ts-mode-navigation--setup ()
  "Configure navigation for the current buffer."
  (setq-local treesit-thing-settings json-ts-mode-navigation--settings))

;;;; Imenu

(defun json-ts-mode-imenu--name (node)
  "Return the source name of NODE, or nil if it has no name."
  (when (json-ts-mode-navigation--defun-p node)
    (let ((name (treesit-node-child-by-field-name node "name")))
      (when (and name (< (treesit-node-start name) (treesit-node-end name))
                 (not (treesit-node-check name 'missing)))
        (treesit-node-text name t)))))

(defconst json-ts-mode-imenu--settings
  '(("Property" "^member$" json-ts-mode-imenu--name nil))
  "Tree-sitter Imenu settings for JSON and JSONC.")

(defun json-ts-mode-imenu--setup ()
  "Configure Imenu for the current buffer."
  (setq-local treesit-defun-name-function #'json-ts-mode-imenu--name)
  (setq-local treesit-simple-imenu-settings json-ts-mode-imenu--settings))

;;;; Indentation

(defcustom json-ts-mode-indent-offset 2
  "Number of spaces for each indentation level."
  :type 'natnum
  :group 'json-ts)

(defun json-ts-mode-indent--content-p (_node parent bol)
  "Return non-nil when BOL is inside string or comment content under PARENT."
  (let ((owner (treesit-parent-until
                parent "^\\(?:string\\|single_line_comment\\|multi_line_comment\\)$" t)))
    (and owner (> bol (treesit-node-start owner)))))

(defun json-ts-mode-indent--member (node parent bol)
  "Return the member at BOL represented by NODE and PARENT, if any."
  (if (equal (treesit-node-type parent) "member")
      parent
    (when (and (> bol (point-min))
               (or (not node) (= (treesit-node-start node) (treesit-node-end node))))
      (treesit-parent-until
       (treesit-node-on (1- bol) bol treesit-primary-parser) "^member$" t))))

(defun json-ts-mode-indent--member-value-p (node parent bol)
  "Return non-nil when NODE and PARENT place BOL at a member value."
  (let* ((member (json-ts-mode-indent--member node parent bol))
         (value (treesit-node-child-by-field-name member "value"))
         (colon (and member (cdr (car (treesit-query-capture member '(":" @colon)))))))
    (and colon (<= (treesit-node-end colon) bol)
         (or (not value) (<= bol (treesit-node-start value))))))

(defun json-ts-mode-indent--member-bol (node parent bol)
  "Return the indentation position of the member at NODE, PARENT and BOL."
  (save-excursion
    (goto-char (treesit-node-start (json-ts-mode-indent--member node parent bol)))
    (back-to-indentation)
    (point)))

(defconst json-ts-mode-indent--rules
  (let ((rules '((json-ts-mode-indent--content-p no-indent 0)
                 ((query ((object "}" @close) (array "]" @close))) parent-bol 0)
                 (json-ts-mode-indent--member-value-p
                  json-ts-mode-indent--member-bol json-ts-mode-indent-offset)
                 ((parent-is "^object$") parent-bol json-ts-mode-indent-offset)
                 ((parent-is "^array$") parent-bol json-ts-mode-indent-offset)
                 ((parent-is "^json_text$") column-0 0))))
    (mapcar (lambda (source) (cons (car source) rules)) json-ts-mode--grammar-sources))
  "Tree-sitter indentation rules for JSON and JSONC.")

(defun json-ts-mode-indent--setup ()
  "Configure indentation for the current buffer."
  (setq-local treesit-simple-indent-rules json-ts-mode-indent--rules))

;;;; Mode

(defun json-ts-mode--setup ()
  "Configure `json-ts-mode' in the current buffer."
  (let ((language (json-ts-mode--language)))
    (json-ts-mode--ensure-grammar language)
    (setq-local treesit-primary-parser (treesit-parser-create language)))
  (json-ts-mode-syntax--setup)
  (json-ts-mode-comment--setup)
  (json-ts-mode-electric-pair--setup)
  (json-ts-mode-font-lock--setup)
  (json-ts-mode-navigation--setup)
  (json-ts-mode-imenu--setup)
  (json-ts-mode-indent--setup)
  (treesit-major-mode-setup)
  (json-ts-mode-update--setup))

;;;###autoload
(define-derived-mode json-ts-mode prog-mode "JSON-TS"
  "Major mode for editing JSON and JSONC."
  :syntax-table json-ts-mode-syntax-table
  :group 'json-ts
  (json-ts-mode--setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jsonc?\\'" . json-ts-mode))

(provide 'json-ts-mode)

;;; json-ts-mode.el ends here

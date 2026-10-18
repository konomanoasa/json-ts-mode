;;; json-ts-mode-test.el --- Tests for json-ts-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 konomanoasa
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

;;; Code:

(require 'ert)
(require 'imenu)
(require 'json-ts-mode)

(dolist (language '(json jsonc jsonc_trailing))
  (unless (treesit-ready-p language t)
    (error "The %s grammar is required to run the tests" language)))

;;;; Helpers

(defmacro json-ts-mode-test--with-directory (&rest body)
  "Evaluate BODY in an isolated EditorConfig directory."
  (declare (indent 0) (debug (body)))
  `(let ((default-directory
          (file-name-as-directory (make-temp-file "json-ts-mode-" t))))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name ".editorconfig")
             (insert "root = true\n"))
           ,@body)
       (delete-directory default-directory t))))

(defmacro json-ts-mode-test--with-source (language source &rest body)
  "Evaluate BODY with SOURCE in the mode for LANGUAGE."
  (declare (indent 2) (debug (form form body)))
  `(json-ts-mode-test--with-directory
    (with-temp-file (expand-file-name ".editorconfig")
      (insert "root = true\n[*.json]\njson_dialect = "
              (symbol-name ,language) "\n"))
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name "example.json"))
      (insert ,source)
      (json-ts-mode)
      (ert-info ((format "%s, font-lock level %s: %S"
                         (treesit-parser-language treesit-primary-parser)
                         treesit-font-lock-level
                         (buffer-substring-no-properties (point-min) (point-max))))
                ,@body))))

(defun json-ts-mode-test--position (fragment)
  "Return the first position of FRAGMENT in the buffer."
  (save-excursion
    (goto-char (point-min))
    (unless (search-forward fragment nil t)
      (ert-fail (format "Missing fixture fragment: %S" fragment)))
    (- (point) (length fragment))))

(defun json-ts-mode-test--face (fragment &optional offset)
  "Return the face at OFFSET into FRAGMENT."
  (get-text-property (+ (json-ts-mode-test--position fragment) (or offset 0)) 'face))

(defun json-ts-mode-test--indent (language source &optional offset)
  "Indent SOURCE twice for LANGUAGE with OFFSET, checking stability."
  (json-ts-mode-test--with-source language source
                                  (setq-local indent-tabs-mode nil)
                                  (when offset (setq-local json-ts-mode-indent-offset offset))
                                  (indent-region (point-min) (point-max))
                                  (let ((indented (buffer-substring-no-properties (point-min) (point-max))))
                                    (indent-region (point-min) (point-max))
                                    (should (equal (buffer-substring-no-properties (point-min) (point-max)) indented))
                                    indented)))

(defun json-ts-mode-test--buffer-state ()
  "Return refreshed font-lock and syntax classifications."
  (font-lock-ensure)
  (syntax-propertize (point-max))
  (let (state)
    (dotimes (offset (- (point-max) (point-min)))
      (let ((position (+ (point-min) offset)))
        (push (list (get-text-property position 'face)
                    (syntax-after position)
                    (nth 3 (syntax-ppss position))
                    (nth 4 (syntax-ppss position))) state)))
    (nreverse state)))

(defun json-ts-mode-test--should-match-fresh-buffer (&optional level compare-tree)
  "Compare classifications at LEVEL and optionally COMPARE-TREE with fresh parsing."
  (let ((source (buffer-substring-no-properties (point-min) (point-max)))
        (state (json-ts-mode-test--buffer-state))
        (tree (when compare-tree
                (treesit-node-string (treesit-parser-root-node treesit-primary-parser))))
        (file buffer-file-name)
        (level (or level treesit-font-lock-level)))
    (with-temp-buffer
      (setq buffer-file-name file)
      (insert source)
      (let ((treesit-font-lock-level level)) (json-ts-mode))
      (should (equal state (json-ts-mode-test--buffer-state)))
      (when compare-tree
        (should (equal tree (treesit-node-string
                             (treesit-parser-root-node treesit-primary-parser))))))))

;;;; Mode Selection

(ert-deftest json-ts-mode-selects-dialects-independently-of-editorconfig-mode ()
  (dolist (enabled '(nil t))
    (pcase-dolist (`(,dialect ,filename ,expected)
                   '(("json" "example.jsonc" json) ("jsonc" "example.json" jsonc)
                     ("jsonc_trailing" "example.json" jsonc_trailing)
                     (nil "example.json" json) (nil "example.jsonc" jsonc)
                     ("unset" "example.jsonc" jsonc)
                     (nil "tsconfig.json" jsonc_trailing)
                     (nil "jsconfig.json" jsonc_trailing)
                     ("json" "tsconfig.json" json)
                     ("jsonc" "jsconfig.json" jsonc)
                     ("unset" "tsconfig.json" jsonc_trailing)
                     ("unset" "jsconfig.json" jsonc_trailing)))
      (json-ts-mode-test--with-directory
       (with-temp-file (expand-file-name ".editorconfig")
         (insert "root = true\n[*]\n")
         (when dialect (insert "json_dialect = " dialect "\n")))
       (let ((file (expand-file-name filename))
             (previous editorconfig-mode))
         (with-temp-file file (insert "// note\n{\"a\": 1,}\n"))
         (unwind-protect
             (progn
               (editorconfig-mode (if enabled 1 -1))
               (with-current-buffer (find-file-noselect file)
                 (unwind-protect
                     (progn
                       (should (eq major-mode 'json-ts-mode))
                       (should (eq (treesit-parser-language treesit-primary-parser) expected))
                       (should (equal comment-start (unless (eq expected 'json) "// ")))
                       (should (eq (not (treesit-search-subtree
                                         (treesit-parser-root-node treesit-primary-parser)
                                         "^syntax_issue$"))
                                   (eq expected 'jsonc_trailing)))
                       (should (eq editorconfig-mode enabled)))
                   (kill-buffer))))
           (editorconfig-mode (if previous 1 -1))))))))

(ert-deftest json-ts-mode-matches-known-file-basenames-exactly ()
  (json-ts-mode-test--with-directory
   (make-directory "nested")
   (make-directory "tsconfig.json")
   (pcase-dolist (`(,filename ,expected)
                  '(("nested/tsconfig.json" jsonc_trailing)
                    ("nested/jsconfig.json" jsonc_trailing)
                    ("Tsconfig.json" json)
                    ("tsconfig.json/example.json" json)
                    ("tsconfig.jsonc" jsonc)))
     (with-temp-buffer
       (setq buffer-file-name (expand-file-name filename))
       (insert "{}")
       (json-ts-mode)
       (should (eq (treesit-parser-language treesit-primary-parser) expected))))))

(ert-deftest json-ts-mode-defaults-to-json-without-a-file ()
  (json-ts-mode-test--with-directory
   (with-temp-file (expand-file-name ".editorconfig")
     (insert "root = true\n[*]\njson_dialect = jsonc_trailing\n"))
   (with-temp-buffer
     (insert "/* comment */ {}")
     (json-ts-mode)
     (should (eq (treesit-parser-language treesit-primary-parser) 'json)))))

(ert-deftest json-ts-mode-rejects-invalid-editorconfig-values-before-parser-creation ()
  (dolist (dialect '("JSONC" ""))
    (json-ts-mode-test--with-directory
     (with-temp-file (expand-file-name ".editorconfig")
       (insert "root = true\n[*]\njson_dialect = " dialect "\n"))
     (dolist (filename '("example.json" "tsconfig.json"))
       (with-temp-buffer
         (setq buffer-file-name (expand-file-name filename))
         (should (equal (error-message-string
                         (should-error (json-ts-mode) :type 'user-error))
                        (format "Invalid EditorConfig json_dialect: %S" dialect)))
         (should-not (treesit-parser-list nil nil t)))))))

(ert-deftest json-ts-mode-reselects-language-only-on-mode-restart ()
  (json-ts-mode-test--with-source 'json "// note\n[1,]"
                                  (let ((parser treesit-primary-parser))
                                    (with-temp-file (expand-file-name ".editorconfig")
                                      (insert "root = true\n[*.json]\njson_dialect = jsonc_trailing\n"))
                                    (font-lock-ensure)
                                    (should (eq treesit-primary-parser parser))
                                    (json-ts-mode)
                                    (should (eq (treesit-parser-language treesit-primary-parser) 'jsonc_trailing))
                                    (should (equal (treesit-parser-list) (list treesit-primary-parser)))
                                    (should-not (treesit-search-subtree
                                                 (treesit-parser-root-node treesit-primary-parser) "^syntax_issue$")))))

;;;; Syntax

(ert-deftest json-ts-mode-classifies-only-owned-brackets-and-string-fences ()
  (dolist (language '(json jsonc jsonc_trailing))
    (json-ts-mode-test--with-source language "{\"a\": [\"x\\\"[]{}\\\\y\", 1]} () ' ` \\"
                                    (syntax-propertize (point-max))
                                    (should (= (syntax-class (syntax-after 1)) 4))
                                    (should (= (syntax-class (syntax-after (json-ts-mode-test--position "[\"x"))) 4))
                                    (should (= (syntax-class (syntax-after (json-ts-mode-test--position "\"a\""))) 15))
                                    (should (nth 3 (syntax-ppss (json-ts-mode-test--position "[]{}"))))
                                    (dolist (fragment '("[]{}" "\\\\" "()" "'" "`"))
                                      (dotimes (offset (length fragment))
                                        (should (= (syntax-class (syntax-after
                                                                  (+ (json-ts-mode-test--position fragment) offset))) 1))))
                                    (should-not (nth 3 (syntax-ppss (point-max)))))))

(ert-deftest json-ts-mode-matches-delimiters-by-owner-without-inventing-closings ()
  (dolist (language '(json jsonc jsonc_trailing))
    (json-ts-mode-test--with-source language "{\"a\": [1]}"
                                    (goto-char 1)
                                    (should (equal (funcall show-paren-data-function) '(1 2 10 11)))
                                    (goto-char 7)
                                    (should (equal (funcall show-paren-data-function) '(7 8 9 10))))
    (json-ts-mode-test--with-source language "{\"a\": [1}"
                                    (goto-char 7)
                                    (should-not (funcall show-paren-data-function))
                                    (goto-char 1)
                                    (should (equal (funcall show-paren-data-function) '(1 2 9 10))))))

;;;; Comment Commands

(ert-deftest json-ts-mode-rejects-json-comment-keys-without-prompting-or-editing ()
  (let ((reader (symbol-function 'read-string)))
    (unwind-protect
        (progn
          (fset 'read-string (lambda (&rest _) (ert-fail "Unexpected comment syntax prompt")))
          (dolist (source '("" "{\"a\":1}\n"))
            (with-temp-buffer
              (insert source)
              (json-ts-mode)
              (dolist (active '(nil t))
                (let ((transient-mark-mode t)
                      (mark-active active))
                  (set-mark (point-min))
                  (goto-char (point-max))
                  (dolist (key '("M-;" "C-x C-;"))
                    (should (equal (error-message-string
                                    (should-error
                                     (call-interactively (key-binding (kbd key)))
                                     :type 'user-error))
                                   "JSON does not support comments"))
                    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                                   source))))))))
      (fset 'read-string reader))))

(ert-deftest json-ts-mode-json-comment-remaps-do-not-affect-jsonc-buffers ()
  (dolist (language '(jsonc jsonc_trailing))
    (json-ts-mode-test--with-source language "true\n"
                                    (with-temp-buffer (json-ts-mode))
                                    (should (eq (key-binding (kbd "M-;")) 'comment-dwim))
                                    (should (eq (key-binding (kbd "C-x C-;")) 'comment-line))
                                    (let ((transient-mark-mode t))
                                      (set-mark (point-min))
                                      (goto-char (point-max))
                                      (activate-mark)
                                      (call-interactively (key-binding (kbd "M-;"))))
                                    (should (equal (buffer-substring-no-properties (point-min) (point-max)) "// true\n"))
                                    (deactivate-mark)
                                    (goto-char (point-min))
                                    (call-interactively (key-binding (kbd "C-x C-;")))
                                    (should (equal (buffer-substring-no-properties (point-min) (point-max)) "true\n")))))

(ert-deftest json-ts-mode-refreshes-comment-remaps-on-language-reselection ()
  (json-ts-mode-test--with-source 'json "{}"
                                  (dolist (language '(jsonc json jsonc_trailing json))
                                    (with-temp-file (expand-file-name ".editorconfig")
                                      (insert "root = true\n[*.json]\njson_dialect = " (symbol-name language) "\n"))
                                    (json-ts-mode)
                                    (dolist (command '(comment-dwim comment-line))
                                      (should (eq (command-remapping command)
                                                  (and (eq language 'json) 'json-ts-mode-comment--unsupported)))))))

;;;; Electric Pair

(ert-deftest json-ts-mode-restricts-pair-newlines-to-real-array-and-object-delimiters ()
  (let ((electric-pair-open-newline-between-pairs t))
    (pcase-dolist (`(,language ,source ,expected)
                   '((json "[|]" "[\n\n]") (json "{|}" "{\n\n}")
                     (json "\"[|]\"" "\"[\n]\"") (json "\"|\"" "\"\n\"")
                     (jsonc "// {|}" "// {\n}")
                     (jsonc_trailing "/* [|] */" "/* [\n] */")))
      (json-ts-mode-test--with-source language source
                                      (electric-indent-local-mode -1)
                                      (electric-pair-local-mode 1)
                                      (goto-char (json-ts-mode-test--position "|"))
                                      (delete-char 1)
                                      (call-interactively (key-binding (kbd "RET")))
                                      (should (equal (buffer-substring-no-properties (point-min) (point-max)) expected))))))

(ert-deftest json-ts-mode-pairs-and-indents-on-return ()
  (let ((electric-pair-open-newline-between-pairs t))
    (dolist (language '(json jsonc jsonc_trailing))
      (pcase-dolist (`(,opening ,expected)
                     '((?{ "{\n  \n}") (?\[ "[\n  \n]")))
        (json-ts-mode-test--with-source language ""
                                        (setq-local indent-tabs-mode nil)
                                        (electric-pair-local-mode 1)
                                        (electric-indent-local-mode 1)
                                        (let ((last-command-event opening)) (self-insert-command 1))
                                        (call-interactively (key-binding (kbd "RET")))
                                        (should (equal (buffer-substring-no-properties (point-min) (point-max)) expected))
                                        (should (= (current-column) 2)))))))

(ert-deftest json-ts-mode-does-not-match-or-expand-brackets-in-a-narrowed-comment ()
  (let ((electric-pair-open-newline-between-pairs t))
    (dolist (language '(jsonc jsonc_trailing))
      (json-ts-mode-test--with-source language "/* head\n[|]\n*/"
                                      (electric-indent-local-mode -1)
                                      (electric-pair-local-mode 1)
                                      (narrow-to-region (json-ts-mode-test--position "[")
                                                        (json-ts-mode-test--position "*/"))
                                      (goto-char (point-min))
                                      (should-not (funcall show-paren-data-function))
                                      (goto-char (json-ts-mode-test--position "|"))
                                      (delete-char 1)
                                      (call-interactively (key-binding (kbd "RET")))
                                      (should (equal (buffer-substring-no-properties (point-min) (point-max)) "[\n]\n"))))))

;;;; Navigation

(ert-deftest json-ts-mode-navigates-members-names-values-and-shared-boundaries ()
  (dolist (language '(json jsonc jsonc_trailing))
    (json-ts-mode-test--with-source language "{\"a\": [12, 34], \"b\": \"text\"}"
                                    (pcase-dolist (`(,start ,count ,end)
                                                   '((1 1 29) (2 1 15) (3 1 5) (5 -1 2)
                                                     (7 1 15) (8 1 10) (9 1 10) (10 -1 8)
                                                     (12 1 14) (14 -1 12) (15 -1 2)
                                                     (17 1 28) (23 1 28) (28 -1 17)
                                                     (29 -1 1) (2 2 28) (28 -2 2)))
                                      (goto-char start)
                                      (forward-sexp count)
                                      (should (= (point) end))))))

(ert-deftest json-ts-mode-navigation-excludes-string-and-number-fragments ()
  (json-ts-mode-test--with-source 'json "{\"a\\u0041\": -12.3e+4}"
                                  (pcase-dolist (`(,start ,count ,end)
                                                 '((3 1 11) (7 1 11) (7 -1 2) (15 1 21) (19 -1 13)))
                                    (goto-char start)
                                    (forward-sexp count)
                                    (should (= (point) end)))))

(ert-deftest json-ts-mode-navigates-multiline-defuns-and-refreshes-after-edits ()
  (dolist (language '(json jsonc jsonc_trailing))
    (json-ts-mode-test--with-source language
                                    "{\n  \"a\": {\"inner\":1},\n  \"b\": [2]\n}\n"
                                    (goto-char (point-max))
                                    (beginning-of-defun)
                                    (should (= (point) 23))
                                    (end-of-defun)
                                    (should (= (point) 34))
                                    (goto-char (json-ts-mode-test--position "\"b\": [2]"))
                                    (delete-char (length "\"b\": [2]"))
                                    (goto-char (point-max))
                                    (beginning-of-defun)
                                    (should (= (point) 3))))
  (json-ts-mode-test--with-source 'json "[\n  1,\n  {\"a\":2},\n  [3]\n]\n"
                                  (goto-char (point-max))
                                  (beginning-of-defun)
                                  (should (= (point) 19))
                                  (end-of-defun)
                                  (should (= (point) 25))))

;;;; Imenu

(ert-deftest json-ts-mode-imenu-preserves-quotes-escapes-empty-and-duplicate-names ()
  (dolist (language '(json jsonc jsonc_trailing))
    (json-ts-mode-test--with-source language
                                    "{\"a\": {\"inner\":1}, \"\": 2, \"a\\u0041\": 3, \"a\":4, :5} {\"extra\":6}"
                                    (let ((index (funcall imenu-create-index-function)))
                                      (should (equal (mapcar #'car index) '("Property")))
                                      (should (equal (mapcar #'car (cdar index)) '("\"a\"" "\"\"" "\"a\\u0041\"" "\"a\""))))
                                    (goto-char (json-ts-mode-test--position "\"\": 2"))
                                    (delete-char 2)
                                    (insert "\"new\"")
                                    (should (equal (mapcar #'car (cdar (funcall imenu-create-index-function)))
                                                   '("\"a\"" "\"new\"" "\"a\\u0041\"" "\"a\""))))))

(ert-deftest json-ts-mode-imenu-has-no-array-or-nameless-entries ()
  (dolist (source '("[{\"a\":1}]" "{:1}" "true" "{}" "{1:2}"))
    (json-ts-mode-test--with-source 'json source
                                    (should-not (funcall imenu-create-index-function)))))

;;;; Indentation

(ert-deftest json-ts-mode-indents-containers-and-member-values-stably ()
  (dolist (language '(json jsonc jsonc_trailing))
    (should (equal (json-ts-mode-test--indent language
                                              "{\n\"a\":\n[\n1,\n{\n\"b\":\ntrue\n}\n],\n\"c\": null\n}\n" 3)
                   "{\n   \"a\":\n      [\n         1,\n         {\n            \"b\":\n               true\n         }\n      ],\n   \"c\": null\n}\n"))))

(ert-deftest json-ts-mode-indents-incomplete-containers-and-recovery-delimiters ()
  (dolist (language '(json jsonc jsonc_trailing))
    (pcase-dolist (`(,source ,expected)
                   '(("[\n1\n" "[\n  1\n")
                     ("{\n\"a\": [\n1\n}\n" "{\n  \"a\": [\n    1\n}\n")))
      (should (equal (json-ts-mode-test--indent language source) expected)))))

(ert-deftest json-ts-mode-preserves-comment-body-and-string-content-whitespace ()
  (dolist (language '(jsonc jsonc_trailing))
    (should (equal (json-ts-mode-test--indent language
                                              "{\n/* note\n   text\n\n     more\n */\n\"a\": \"  x  \"\n}\n")
                   "{\n  /* note\n   text\n\n     more\n */\n  \"a\": \"  x  \"\n}\n"))))

(ert-deftest json-ts-mode-indents-blank-lines-and-enter-from-their-cst-owner ()
  (dolist (language '(json jsonc jsonc_trailing))
    (pcase-dolist (`(,source ,column)
                   '(("{|}" 0) ("[|]" 0) ("{\n  \"a\":|\n}" 4)
                     ("{\n  \"a\": [1],|\n}" 2) ("|" 0)
                     ("{\"a\":|" 2) ("{|" 2) ("[|" 2)))
      (json-ts-mode-test--with-source language source
                                      (setq-local indent-tabs-mode nil)
                                      (setq-local electric-pair-open-newline-between-pairs nil)
                                      (electric-indent-local-mode 1)
                                      (electric-pair-local-mode -1)
                                      (goto-char (json-ts-mode-test--position "|"))
                                      (delete-char 1)
                                      (call-interactively (key-binding (kbd "RET")))
                                      (should (= (current-column) column))))
    (pcase-dolist (`(,source ,column)
                   '(("{\n|\n}" 2) ("[\n|\n]" 2) ("{\n  \"a\":\n|\n}" 4)
                     ("{\n  \"a\":\n|" 4) ("|" 0)))
      (json-ts-mode-test--with-source language source
                                      (setq-local indent-tabs-mode nil)
                                      (goto-char (json-ts-mode-test--position "|"))
                                      (delete-char 1)
                                      (indent-according-to-mode)
                                      (should (= (current-column) column))))))

(ert-deftest json-ts-mode-preserves-blank-comment-content-on-direct-indentation ()
  (dolist (language '(jsonc jsonc_trailing))
    (json-ts-mode-test--with-source language "{\n/* head\n   |\ntext\n*/\n}"
                                    (goto-char (json-ts-mode-test--position "|"))
                                    (delete-char 1)
                                    (indent-according-to-mode)
                                    (should (= (current-column) 3)))))

;;;; Updates

(ert-deftest json-ts-mode-refreshes-edited-owners-without-parser-notifications ()
  (dolist (language '(json jsonc jsonc_trailing))
    (dolist (narrow '(nil t))
      (ert-info ((format "Narrowed: %s" narrow))
        (let ((treesit-font-lock-level 4))
          (json-ts-mode-test--with-source language "{\n\"a\": 1\n}\n"
                                          (rename-buffer "json-ts-mode-test" t)
                                          (let ((noninteractive nil)) (font-lock-mode 1))
                                          (json-ts-mode-test--buffer-state)
                                          (should (get-text-property 3 'fontified))
                                          (let ((parser treesit-primary-parser))
                                            (should-not (treesit-node-check (treesit-parser-root-node parser) 'has-error))
                                            (dolist (notifier (treesit-parser-notifiers parser))
                                              (treesit-parser-remove-notifier parser notifier))
                                            (goto-char (json-ts-mode-test--position "1"))
                                            (when narrow
                                              (narrow-to-region (line-beginning-position) (line-end-position)))
                                            (insert "2")
                                            (widen)
                                            (should (eq parser treesit-primary-parser))
                                            (should-not (get-text-property 3 'fontified))
                                            (json-ts-mode-test--should-match-fresh-buffer 4 t))))))))

(ert-deftest json-ts-mode-reparses-a-bom-moved-to-buffer-start ()
  (dolist (language '(json jsonc jsonc_trailing))
    (dolist (narrow '(nil t))
      (ert-info ((format "Narrowed: %s" narrow))
        (let ((treesit-font-lock-level 4))
          (json-ts-mode-test--with-source language "[]\n\ufeff[1]\n"
                                          (json-ts-mode-test--buffer-state)
                                          (let ((parser treesit-primary-parser))
                                            (when narrow (narrow-to-region 1 5))
                                            (delete-region 1 4)
                                            (widen)
                                            (should-not (eq parser treesit-primary-parser))
                                            (should (equal (treesit-parser-list) (list treesit-primary-parser)))
                                            (should (equal (buffer-substring-no-properties (point-min) (point-max))
                                                           "\ufeff[1]\n"))
                                            (json-ts-mode-test--buffer-state)
                                            (should-not (get-text-property 1 'face))
                                            (should (eq (json-ts-mode-test--face "1") 'font-lock-number-face))
                                            (json-ts-mode-test--should-match-fresh-buffer 4 t))))))))

(ert-deftest json-ts-mode-updates-font-lock-and-syntax-like-a-fresh-buffer ()
  (let ((treesit-font-lock-level 4))
    (dolist (language '(json jsonc jsonc_trailing))
      (pcase-dolist (`(,source ,old ,new)
                     '(("{\"a\": [1]}" "[1]" "\"[1]\"")
                       ("{\"a\": \"[1]\"}" "\"[1]\"" "[1]")
                       ("\"x\\q\"" "\\q" "\\u0041")
                       ("\"x\\u0041\"" "0041" "12")
                       ("\"open" "open" "closed\"")
                       ("\"closed\"\ntrue" "closed\"" "open")
                       ("// note\ntrue" "//" "/*")
                       ("/* note\ntrue" "note" "note */")
                       ("/* note */ [1]" "*/" "")
                       ("{\"a\": \"あ\\u12\", \"b\": true}" "\\u12" "\\u0041")))
        (json-ts-mode-test--with-source language source
                                        (json-ts-mode-test--buffer-state)
                                        (goto-char (json-ts-mode-test--position old))
                                        (delete-char (length old))
                                        (insert new)
                                        (json-ts-mode-test--should-match-fresh-buffer))))))

(ert-deftest json-ts-mode-retains-cst-classification-when-narrowed-and-edited ()
  (let ((treesit-font-lock-level 4))
    (dolist (language '(jsonc jsonc_trailing))
      (json-ts-mode-test--with-source language "/* head\n  body\n*/\n{\"a\": \"x\"}"
                                      (json-ts-mode-test--buffer-state)
                                      (let ((begin (json-ts-mode-test--position "  body"))
                                            (end (json-ts-mode-test--position "*/")))
                                        (save-restriction
                                          (narrow-to-region begin end)
                                          (goto-char (point-min))
                                          (search-forward "body")
                                          (insert " text")
                                          (font-lock-ensure)
                                          (syntax-propertize (point-max))
                                          (should (eq (get-text-property (1- (point-max)) 'face) 'font-lock-comment-face))
                                          (let ((position (point-max)))
                                            (save-restriction
                                              (widen)
                                              (should (nth 4 (syntax-ppss position)))))))
                                      (font-lock-flush)
                                      (json-ts-mode-test--should-match-fresh-buffer)))))

(provide 'json-ts-mode-test)

;;; json-ts-mode-test.el ends here

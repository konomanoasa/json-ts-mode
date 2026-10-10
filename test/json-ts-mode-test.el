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
(require 'loaddefs-gen)
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

(defun json-ts-mode-test--should-match-fresh-buffer ()
  "Compare current classifications against a fresh buffer."
  (let ((source (buffer-substring-no-properties (point-min) (point-max)))
        (state (json-ts-mode-test--buffer-state))
        (file buffer-file-name)
        (level treesit-font-lock-level))
    (with-temp-buffer
      (setq buffer-file-name file)
      (insert source)
      (let ((treesit-font-lock-level level)) (json-ts-mode))
      (should (equal state (json-ts-mode-test--buffer-state))))))

;;;; Grammar

(ert-deftest json-ts-mode-respects-grammar-sources-and-install-policy ()
  (let ((ensure (symbol-function 'treesit-ensure-installed)) received)
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed
                (lambda (language)
                  (setq received (list (assq language treesit-language-source-alist)
                                       treesit-auto-install-grammar))
                  t))
          (dolist (source json-ts-mode--grammar-sources)
            (let* ((language (car source))
                   (custom (list language "/local/grammar" :revision "custom")))
              (dolist (configured (list nil (list custom)))
                (dolist (policy '(nil ask always))
                  (let ((treesit-language-source-alist configured)
                        (treesit-auto-install-grammar policy))
                    (should (json-ts-mode--ensure-grammar language))
                    (should (equal received (list (if configured custom source) policy)))
                    (should (eq treesit-language-source-alist configured))))))))
      (fset 'treesit-ensure-installed ensure))))

(ert-deftest json-ts-mode-checks-only-selected-grammar ()
  (let ((ensure (symbol-function 'treesit-ensure-installed)) received)
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed (lambda (language) (push language received) t))
          (dolist (language '(json jsonc jsonc_trailing))
            (setq received nil)
            (json-ts-mode-test--with-source language "{}"
              (should (equal received (list language)))
              (should (eq (treesit-parser-language treesit-primary-parser) language))
              (should (equal (treesit-node-type
                              (treesit-parser-root-node treesit-primary-parser)) "json_text"))
              (json-ts-mode)
              (should (equal (treesit-parser-list) (list treesit-primary-parser))))))
      (fset 'treesit-ensure-installed ensure))))

(ert-deftest json-ts-mode-reports-unavailable-grammar ()
  (let ((ensure (symbol-function 'treesit-ensure-installed)))
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed (lambda (_language) nil))
          (with-temp-buffer
            (should (equal (error-message-string
                            (should-error (json-ts-mode) :type 'user-error))
                           "Tree-sitter grammar ‘json’ is unavailable"))
            (should-not (treesit-parser-list))))
      (fset 'treesit-ensure-installed ensure))))

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
                     ("JSCONFIG.json" json)
                     ("my-tsconfig.json" json)
                     ("tsconfig.dev.json" json)
                     ("jsconfig.json.backup" json)
                     ("tsconfig.json/example.json" json)
                     ("tsconfig.jsonc" jsonc)))
      (with-temp-buffer
        (setq buffer-file-name (expand-file-name filename))
        (insert "{}")
        (json-ts-mode)
        (should (eq (treesit-parser-language treesit-primary-parser) expected))))))

(ert-deftest json-ts-mode-resolves-editorconfig-hierarchy-and-unset ()
  (json-ts-mode-test--with-directory
    (with-temp-file (expand-file-name ".editorconfig")
      (insert "root = true\n[*.jsonc]\njson_dialect = jsonc_trailing\n"))
    (make-directory "child")
    (dolist (dialect '("json" "unset"))
      (with-temp-file (expand-file-name "child/.editorconfig")
        (insert "[*.jsonc]\njson_dialect = " dialect "\n"))
      (with-temp-buffer
        (setq buffer-file-name (expand-file-name "child/example.jsonc"))
        (json-ts-mode)
        (should (eq (treesit-parser-language treesit-primary-parser)
                    (if (equal dialect "unset") 'jsonc 'json)))))))

(ert-deftest json-ts-mode-defaults-to-json-without-a-file ()
  (json-ts-mode-test--with-directory
    (with-temp-file (expand-file-name ".editorconfig")
      (insert "root = true\n[*]\njson_dialect = jsonc_trailing\n"))
    (with-temp-buffer
      (insert "/* comment */ {}")
      (json-ts-mode)
      (should (eq (treesit-parser-language treesit-primary-parser) 'json)))))

(ert-deftest json-ts-mode-rejects-invalid-editorconfig-values-before-parser-creation ()
  (dolist (dialect '("json5" "JSONC" "invalid" ""))
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

(ert-deftest json-ts-mode-registers-only-json-and-jsonc-files ()
  (let (patterns)
    (dolist (entry auto-mode-alist)
      (when (eq (cdr entry) 'json-ts-mode) (push (car entry) patterns)))
    (should (equal patterns '("\\.jsonc?\\'")))
    (dolist (file '("example.json" "example.jsonc"))
      (should (string-match-p (car patterns) file)))
    (dolist (file '("example.json5" "example.jsonl" "example.ndjson" "example.jsonc.backup"))
      (should-not (string-match-p (car patterns) file)))))

(ert-deftest json-ts-mode-generates-runtime-autoloads ()
  (let ((output (make-temp-file "json-ts-mode-loaddefs-"))
        (directory (file-name-directory (locate-library "json-ts-mode"))))
    (unwind-protect
        (progn
          (loaddefs-generate directory output nil nil nil t)
          (with-temp-buffer
            (insert-file-contents output)
            (dolist (fragment '("(autoload 'json-ts-mode" "(add-to-list 'auto-mode-alist"))
              (goto-char (point-min))
              (should (search-forward fragment nil t)))))
      (delete-file output))))

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

(ert-deftest json-ts-mode-does-not-fence-incomplete-strings ()
  (dolist (language '(json jsonc jsonc_trailing))
    (dolist (source '("\"open" "\"open\n[1]" "{\"name" "\"\\"))
      (json-ts-mode-test--with-source language source
        (syntax-propertize (point-max))
        (should (= (syntax-class (syntax-after (json-ts-mode-test--position "\""))) 1))
        (should-not (nth 3 (syntax-ppss (point-max))))))))

(ert-deftest json-ts-mode-keeps-real-string-fences-around-syntax-issues ()
  (dolist (language '(json jsonc jsonc_trailing))
    (json-ts-mode-test--with-source language "\"x\\q\\u12y\""
      (syntax-propertize (point-max))
      (should (= (syntax-class (syntax-after 1)) 15))
      (should (nth 3 (syntax-ppss 3)))
      (should-not (nth 3 (syntax-ppss (point-max)))))))

(ert-deftest json-ts-mode-classifies-jsonc-comments-with-cr-lf-and-eof ()
  (dolist (language '(jsonc jsonc_trailing))
    (json-ts-mode-test--with-source language
        "// first\r{\"s\":\"// string\"} // next\r\n/* multi\n[]\r{} */ true // eof"
      (syntax-propertize (point-max))
      (dolist (fragment '("first" "next" "multi" "[]" "{}" "eof"))
        (should (nth 4 (syntax-ppss (json-ts-mode-test--position fragment)))))
      (should-not (nth 4 (syntax-ppss (json-ts-mode-test--position "{\"s"))))
      (should-not (nth 4 (syntax-ppss (json-ts-mode-test--position "// string"))))
      (should-not (nth 4 (syntax-ppss (json-ts-mode-test--position "true"))))
      (should (nth 4 (syntax-ppss (point-max)))))
    (json-ts-mode-test--with-source language "/* open\n\"[]{}\""
      (syntax-propertize (point-max))
      (should (nth 4 (syntax-ppss (point-max)))))))

(ert-deftest json-ts-mode-json-has-no-comment-syntax-or-commands ()
  (json-ts-mode-test--with-source 'json "// text\n/* note */\ntrue"
    (syntax-propertize (point-max))
    (should-not comment-start)
    (dolist (fragment '("text" "note" "true"))
      (should-not (nth 4 (syntax-ppss (json-ts-mode-test--position fragment)))))))

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

(ert-deftest json-ts-mode-comments-and-uncomments-jsonc-through-standard-commands ()
  (dolist (language '(jsonc jsonc_trailing))
    (dolist (source '("true\n" "{\n  \"url\": \"https://example.test\",\n  \"a\": 1\n}\n"))
      (json-ts-mode-test--with-source language source
        (comment-or-uncomment-region (point-min) (point-max))
        (should (equal (buffer-substring-no-properties 1 4) "// "))
        (comment-or-uncomment-region (point-min) (point-max))
        (should (equal (buffer-substring-no-properties (point-min) (point-max)) source))))))

(ert-deftest json-ts-mode-uncomments-block-comments-and-preserves-content-spaces ()
  (dolist (language '(jsonc jsonc_trailing))
    (json-ts-mode-test--with-source language "/* true */"
      (uncomment-region (point-min) (point-max))
      (should (equal (buffer-substring-no-properties (point-min) (point-max)) "true")))))

;;;; Electric Pair

(ert-deftest json-ts-mode-adds-buffer-local-pairs-and-preserves-user-overrides ()
  (let ((electric-pair-pairs '((?% . ?%) (?\[ . ?!)))
        (electric-pair-mode nil))
    (dolist (language '(json jsonc jsonc_trailing))
      (pcase-dolist (`(,opening ,expected)
                     '((?\[ "[!") (?{ "{}") (?\" "\"\"")))
        (json-ts-mode-test--with-source language ""
          (should-not electric-pair-mode)
          (should (local-variable-p 'electric-pair-pairs))
          (should (equal (assq ?% electric-pair-pairs) '(?% . ?%)))
          (electric-pair-local-mode 1)
          (let ((last-command-event opening)) (self-insert-command 1))
          (should (equal (buffer-substring-no-properties (point-min) (point-max)) expected))
          (should (= (point) (1- (point-max)))))))
    (should (equal electric-pair-pairs '((?% . ?%) (?\[ . ?!))))))

(ert-deftest json-ts-mode-does-not-pair-in-comments-and-preserves-inhibit-predicate ()
  (dolist (language '(jsonc jsonc_trailing))
    (dolist (source '("// note " "/* note  */"))
      (dolist (character '(?{ ?\[ ?\"))
        (json-ts-mode-test--with-source language source
          (electric-pair-local-mode 1)
          (goto-char (+ (json-ts-mode-test--position "note ") 5))
          (let ((before (buffer-size)) (last-command-event character))
            (self-insert-command 1)
            (should (= (buffer-size) (1+ before))))))))
  (let* ((calls nil)
         (electric-pair-inhibit-predicate (lambda (character) (push character calls) t)))
    (json-ts-mode-test--with-source 'json ""
      (electric-pair-local-mode 1)
      (let ((last-command-event ?{)) (self-insert-command 1))
      (should (equal (buffer-substring-no-properties (point-min) (point-max)) "{"))
      (should (equal calls '(?{))))))

(ert-deftest json-ts-mode-pairs-quotes-and-skips-the-inserted-closing-quote ()
  (dolist (language '(json jsonc jsonc_trailing))
    (json-ts-mode-test--with-source language "{"
      (electric-pair-local-mode 1)
      (dolist (character '(?\" ?x ?\"))
        (let ((last-command-event character)) (self-insert-command 1)))
      (should (equal (buffer-substring-no-properties (point-min) (point-max)) "{\"x\""))
      (should (eobp)))))

(ert-deftest json-ts-mode-respects-pair-newline-preferences ()
  (let ((calls 0))
    (pcase-dolist (`(,setting ,expected)
                   (list (list nil "{\n}") (list t "{\n\n}")
                         (list (lambda () (setq calls (1+ calls)) nil) "{\n}")
                         (list (lambda () (setq calls (1+ calls)) t) "{\n\n}")))
      (setq calls 0)
      (let ((electric-pair-open-newline-between-pairs setting))
        (json-ts-mode-test--with-source 'json "{}"
          (electric-indent-local-mode -1)
          (electric-pair-local-mode 1)
          (goto-char 2)
          (call-interactively (key-binding (kbd "RET")))
          (should (equal (buffer-substring-no-properties (point-min) (point-max)) expected)))
        (should (eq (> calls 0) (functionp setting)))
        (should (eq electric-pair-open-newline-between-pairs setting))))))

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

(ert-deftest json-ts-mode-pair-settings-remain-independent-between-buffers ()
  (let ((electric-pair-pairs '((?\[ . ?\]) (?{ . ?}) (?\" . ?\")))
        (electric-pair-open-newline-between-pairs nil))
    (json-ts-mode-test--with-source 'json ""
      (should (local-variable-p 'electric-pair-pairs))
      (setq-local electric-pair-pairs '((?{ . ?>)))
      (let ((pairs electric-pair-pairs))
        (json-ts-mode-test--with-source 'jsonc ""
          (should (equal (assq ?{ electric-pair-pairs) '(?{ . ?}))))
        (should (eq electric-pair-pairs pairs))))
    (should (equal electric-pair-pairs '((?\[ . ?\]) (?{ . ?}) (?\" . ?\"))))
    (should-not electric-pair-open-newline-between-pairs)))

(ert-deftest json-ts-mode-inhibits-pairing-in-a-narrowed-comment-body ()
  (dolist (language '(jsonc jsonc_trailing))
    (json-ts-mode-test--with-source language "/* head\nbody\n*/"
      (electric-pair-local-mode 1)
      (narrow-to-region (json-ts-mode-test--position "body")
                        (json-ts-mode-test--position "*/"))
      (goto-char (point-min))
      (let ((before (buffer-size)) (last-command-event ?{))
        (self-insert-command 1)
        (should (= (buffer-size) (1+ before)))))))

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

;;;; Font Lock

(ert-deftest json-ts-mode-fontifies-by-decoration-level ()
  (dolist (language '(json jsonc jsonc_trailing))
    (dolist (level '(1 2 3 4))
      (let ((treesit-font-lock-level level))
        (json-ts-mode-test--with-source language "{\"key\": [true, null, 12, \"x\\n\"]} // note"
          (font-lock-ensure)
          (pcase-dolist (`(,fragment ,minimum ,face)
                         '(("true" 1 font-lock-constant-face) ("null" 1 font-lock-constant-face)
                           ("key" 2 font-lock-property-name-face) ("x" 2 font-lock-string-face)
                           ("12" 3 font-lock-number-face) ("\\n" 3 font-lock-escape-face)
                           ("{" 4 font-lock-bracket-face) (":" 4 font-lock-punctuation-face)
                           ("," 4 font-lock-punctuation-face)))
            (should (eq (json-ts-mode-test--face fragment) (and (>= level minimum) face))))
          (should (eq (json-ts-mode-test--face "note")
                      (and (not (eq language 'json)) 'font-lock-comment-face))))))))

(ert-deftest json-ts-mode-fontifies-every-string-number-and-escape-leaf ()
  (let ((treesit-font-lock-level 4))
    (dolist (language '(json jsonc jsonc_trailing))
      (json-ts-mode-test--with-source language
          "{\"na\\u0041me\": [false, -0.12E+34, 567, \"あ x\\\"\\\\\\/\\b\\f\\n\\r\\t\\u0041\"]}"
        (font-lock-ensure)
        (pcase-dolist (`(,fragment ,face)
                       '(("na" font-lock-property-name-face) ("me" font-lock-property-name-face)
                         ("false" font-lock-constant-face) ("-0.12E+34" font-lock-number-face)
                         ("567" font-lock-number-face) ("あ x" font-lock-string-face)
                         ("\\\"\\\\\\/\\b\\f\\n\\r\\t\\u0041" font-lock-escape-face)))
          (dotimes (offset (length fragment))
            (should (eq (json-ts-mode-test--face fragment offset) face))))))))

(ert-deftest json-ts-mode-fontifies-retained-leaves-and-leaves-unclassified-source-alone ()
  (let ((treesit-font-lock-level 4))
    (dolist (language '(json jsonc jsonc_trailing))
      (json-ts-mode-test--with-source language
          "{1: 01, \"a\": \"x\\q\\u12z\", \"b\": [true,,]} 'bad' tru"
        (font-lock-ensure)
        (pcase-dolist (`(,fragment ,offset ,face)
                       '(("1:" 0 font-lock-number-face) ("01" 1 font-lock-number-face)
                         ("a" 0 font-lock-property-name-face) ("x" 0 font-lock-string-face)
                         ("\\q" 0 font-lock-escape-face) ("\\q" 1 nil)
                         ("\\u12z" 1 font-lock-escape-face) ("\\u12z" 2 font-lock-escape-face)
                         ("\\u12z" 4 nil) ("true,," 4 font-lock-punctuation-face)
                         ("bad" 0 nil) (" tru" 1 nil)))
          (should (eq (json-ts-mode-test--face fragment offset) face)))))))

(ert-deftest json-ts-mode-font-lock-captures-only-real-leaves ()
  (dolist (language '(json jsonc jsonc_trailing))
    (json-ts-mode-test--with-source language "{\"a\": [01, \"x\\q\\u12\", true,]} /* note */"
      (dolist (setting treesit-font-lock-settings)
        (dolist (capture (treesit-query-capture
                          (treesit-parser-root-node treesit-primary-parser) (car setting)))
          (let ((node (cdr capture)))
            (should (= (treesit-node-child-count node) 0))
            (should-not (treesit-node-check node 'missing))
            (should (< (treesit-node-start node) (treesit-node-end node)))))))))

;;;; Navigation

(ert-deftest json-ts-mode-navigates-members-names-values-and-shared-boundaries ()
  (dolist (language '(json jsonc jsonc_trailing))
    (json-ts-mode-test--with-source language "{\"a\": [12, 34], \"b\": \"text\"}"
      (should (equal (mapcar #'car (cdr (assq language treesit-thing-settings))) '(sexp defun)))
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

(ert-deftest json-ts-mode-navigates-only-top-level-container-items-as-defuns ()
  (dolist (language '(json jsonc jsonc_trailing))
    (pcase-dolist (`(,source ,expected)
                   '(("{\"a\": {\"inner\":1}, \"b\": [2]} {\"extra\": 3}"
                      ("\"a\": {\"inner\":1}" "\"b\": [2]"))
                     ("[1, {\"inner\":2}, [3]] [4]" ("1" "{\"inner\":2}" "[3]"))
                     ("true" nil)))
      (json-ts-mode-test--with-source language source
        (should (equal (mapcar (lambda (capture) (treesit-node-text (cdr capture) t))
                               (treesit-query-capture
                                (treesit-parser-root-node treesit-primary-parser)
                                '(((_) @item (:pred json-ts-mode-navigation--defun-p @item)))))
                       expected))
        (when expected
          (goto-char (point-max))
          (beginning-of-defun)
          (should (= (point) (save-excursion
                               (goto-char (json-ts-mode-test--position (car (last expected))))
                               (line-beginning-position))))
          (goto-char (json-ts-mode-test--position (car expected)))
          (end-of-defun)
          (should (>= (point) (+ (json-ts-mode-test--position (car expected))
                                 (length (car expected))))))))))

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

(ert-deftest json-ts-mode-reparses-a-bom-moved-to-buffer-start ()
  (pcase-dolist (`(,language ,file ,properties ,prefix ,body ,fragment ,face)
                 '((json "test.json" "json_dialect = json\n" "[]\n" "[1]\n" "1" font-lock-number-face)
                   (jsonc "test.json" "json_dialect = jsonc\n" "[]\n" "[1]\n" "1" font-lock-number-face)
                   (jsonc_trailing "test.json" "json_dialect = jsonc_trailing\n" "[]\n" "[1]\n" "1" font-lock-number-face)))
    (dolist (narrow '(nil t))
      (ert-info ((format "%s, narrowed: %s" language narrow))
        (let ((default-directory
               (file-name-as-directory (make-temp-file "json-ts-mode-" t))))
          (unwind-protect
              (progn
                (with-temp-file (expand-file-name ".editorconfig")
                  (insert "root = true\n[*]\n" properties))
                (with-temp-buffer
                  (setq buffer-file-name (expand-file-name file))
                  (insert prefix "\ufeff" body)
                  (let ((treesit-font-lock-level 4)) (json-ts-mode))
                  (json-ts-mode-test--buffer-state)
                  (when narrow
                    (narrow-to-region 1 (+ 2 (length prefix))))
                  (delete-region 1 (1+ (length prefix)))
                  (widen)
                  (should (eq (treesit-parser-language treesit-primary-parser) language))
                  (should (equal (treesit-parser-list) (list treesit-primary-parser)))
                  (let ((state (json-ts-mode-test--buffer-state))
                        (tree (treesit-node-string
                               (treesit-parser-root-node treesit-primary-parser)))
                        (source (buffer-substring-no-properties (point-min) (point-max)))
                        (filename buffer-file-name))
                    (should (equal source (concat "\ufeff" body)))
                    (should-not (get-text-property 1 'face))
                    (goto-char (point-min))
                    (search-forward fragment)
                    (should (eq (get-text-property (1- (point)) 'face) face))
                    (with-temp-buffer
                      (setq buffer-file-name filename)
                      (insert source)
                      (let ((treesit-font-lock-level 4)) (json-ts-mode))
                      (should (equal state (json-ts-mode-test--buffer-state)))
                      (should (equal tree (treesit-node-string
                                           (treesit-parser-root-node treesit-primary-parser))))))))
            (delete-directory default-directory t)))))))

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
                       ("[01, 2]" "01" "-1.2e+3")
                       ("[1,]" "," "")
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

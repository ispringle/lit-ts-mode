;;; lit-ts-mode.el --- Lit-aware JS/TS modes with tree-sitter support -*- lexical-binding: t; -*-

;; Author: Ian S. Pringle <ian@dapringles.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (template-literals-ts-mode "0.1.0"))
;; Keywords: languages, javascript, typescript, tree-sitter, lit
;; URL: https://github.com/ispringle/lit-ts-mode
;; License: 0BSD
;; SPDX-License-Identifier: 0BSD

;; Copyright (C) 2026 by Ian S. Pringle ian@dapringles.com
;; 
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;;; Commentary:

;; Lit-aware major modes for JavaScript and TypeScript that use the
;; `lit-html' tree-sitter grammar (from github.com/ispringle/tree-sitter-lit-html)
;; instead of standard `html', and add completion for CSS classes/IDs
;; across `html'/`css' tagged template literals.
;;
;; Requires Emacs 30+ with tree-sitter support and the javascript,
;; typescript, css, and lit-html grammars installed.
;;
;; Usage:
;;   Use `lit-ts-js-mode' instead of `js-ts-mode'
;;   Use `lit-ts-typescript-mode' instead of `typescript-ts-mode'

;;; Code:

(require 'treesit)
(require 'typescript-ts-mode)
(require 'template-literals-ts-mode)

(add-to-list 'treesit-language-source-alist
             '(lit-html "https://github.com/ispringle/tree-sitter-lit-html"))

;;; Candidate collection

(defun lit-ts-mode--collect-css-classes-and-ids ()
  "Collect CSS class and ID names from css parsers in the buffer.
Returns a list of (NAME . TYPE) where TYPE is \"class\" or \"id\"."
  (let ((css-parser (seq-find (lambda (p) (eq (treesit-parser-language p) 'css))
                              (treesit-parser-list)))
        (result nil))
    (when css-parser
      (let ((root (treesit-parser-root-node css-parser)))
        (dolist (capture (treesit-query-capture
                          root
                          '((class_selector (class_name) @class))))
          (when (eq (car capture) 'class)
            (push (cons (treesit-node-text (cdr capture) t) "class") result)))
        (dolist (capture (treesit-query-capture
                          root
                          '((id_selector (id_name) @id))))
          (when (eq (car capture) 'id)
            (push (cons (treesit-node-text (cdr capture) t) "id") result)))))
    (delete-dups result)))

(defun lit-ts-mode--collect-html-classes-and-ids ()
  "Collect class and id attribute values from lit-html parsers.
Returns a list of (NAME . TYPE) where TYPE is \"class\" or \"id\"."
  (let ((html-parser (seq-find (lambda (p) (eq (treesit-parser-language p) 'lit-html))
                               (treesit-parser-list)))
        (result nil))
    (when html-parser
      (let ((root (treesit-parser-root-node html-parser)))
        ;; Class values from quoted attributes
        (dolist (capture (treesit-query-capture
                          root
                          '((attribute
                             (attribute_name) @_name
                             (quoted_attribute_value
                              (attribute_value) @value)
                             (:equal @_name "class")))))
          (when (eq (car capture) 'value)
            (dolist (word (split-string (treesit-node-text (cdr capture) t)))
              (push (cons word "class") result))))
        ;; Class values from bare attributes
        (dolist (capture (treesit-query-capture
                          root
                          '((attribute
                             (attribute_name) @_name
                             (attribute_value) @value
                             (:equal @_name "class")))))
          (when (eq (car capture) 'value)
            (dolist (word (split-string (treesit-node-text (cdr capture) t)))
              (push (cons word "class") result))))
        ;; ID values from quoted attributes
        (dolist (capture (treesit-query-capture
                          root
                          '((attribute
                             (attribute_name) @_name
                             (quoted_attribute_value
                              (attribute_value) @value)
                             (:equal @_name "id")))))
          (when (eq (car capture) 'value)
            (push (cons (treesit-node-text (cdr capture) t) "id") result)))
        ;; ID values from bare attributes
        (dolist (capture (treesit-query-capture
                          root
                          '((attribute
                             (attribute_name) @_name
                             (attribute_value) @value
                             (:equal @_name "id")))))
          (when (eq (car capture) 'value)
            (push (cons (treesit-node-text (cdr capture) t) "id") result)))))
    (delete-dups result)))

(defun lit-ts-mode--merged-candidates ()
  "Return merged list of CSS/HTML class and ID candidates.
Each element is (NAME . TYPE) where TYPE is \"class\" or \"id\"."
  (delete-dups (append (lit-ts-mode--collect-css-classes-and-ids)
                       (lit-ts-mode--collect-html-classes-and-ids))))

;;; Completion-at-point

(defun lit-ts-mode--capf ()
  "Completion at point for CSS selectors and HTML class/id attributes."
  (treesit-update-ranges)
  (let ((lang (treesit-language-at (point))))
    (cond
     ((eq lang 'css)
      (lit-ts-mode--css-capf))
     ((eq lang 'lit-html)
      (lit-ts-mode--html-capf)))))

(defun lit-ts-mode--css-capf ()
  "Completion for CSS selectors after . or #."
  (save-excursion
    (let* ((pos (point))
           (beg (progn
                  (skip-chars-backward "a-zA-Z0-9_-")
                  (point)))
           (trigger-char (when (> beg (point-min))
                           (char-before beg))))
      (when (memq trigger-char '(?. ?#))
        (let* ((is-class (eq trigger-char ?.))
               (candidates (lit-ts-mode--merged-candidates))
               (filtered (mapcar #'car
                                 (seq-filter (lambda (c)
                                               (equal (cdr c)
                                                      (if is-class "class" "id")))
                                             candidates))))
          (list beg pos (delete-dups filtered)))))))

(defun lit-ts-mode--html-capf ()
  "Completion for HTML class/id attribute values."
  (let* ((node (treesit-node-at (point) 'lit-html))
         (attr-node (and node
                         (treesit-parent-until
                          node
                          (lambda (n) (equal (treesit-node-type n) "attribute"))))))
    (when attr-node
      (let* ((attr-name-node (treesit-node-child attr-node 0))
             (attr-name (and attr-name-node
                             (equal (treesit-node-type attr-name-node)
                                    "attribute_name")
                             (treesit-node-text attr-name-node t))))
        (when (member attr-name '("class" "id"))
          (let* ((is-class (equal attr-name "class"))
                 (pos (point))
                 (beg (save-excursion
                        (skip-chars-backward "^ \t\n\"'")
                        (point)))
                 (candidates (lit-ts-mode--merged-candidates))
                 (filtered (mapcar #'car
                                   (seq-filter (lambda (c)
                                                 (equal (cdr c)
                                                        (if is-class "class" "id")))
                                               candidates))))
            (list beg pos (delete-dups filtered))))))))

;;; Setup

(defun lit-ts-mode--setup ()
  "Set up lit-ts-mode with lit-html and css parsers.
Configures `template-literals-ts-tag-grammar-alist' to use the
lit-html grammar for html tagged literals, enables
`template-literals-ts-mode', and adds the capf."
  ;; Configure template-literals-ts-mode to use lit-html instead of html
  (setq-local template-literals-ts-tag-grammar-alist
              '(("html" . lit-html) ("css" . css)))
  (template-literals-ts-mode 1)

  ;; Ensure embedded language font-lock features are active
  (setf (nth 0 treesit-font-lock-feature-list)
        (append (nth 0 treesit-font-lock-feature-list)
                '(selector property tag)))
  (treesit-font-lock-recompute-features)

  (add-hook 'completion-at-point-functions #'lit-ts-mode--capf nil t)

  (font-lock-flush))

;;; Modes

;;;###autoload
(define-derived-mode lit-ts-js-mode js-ts-mode "Lit[JS]"
  "Major mode for Lit-enhanced JavaScript with lit-html template support."
  (lit-ts-mode--setup))

;;;###autoload
(define-derived-mode lit-ts-typescript-mode typescript-ts-mode "Lit[TS]"
  "Major mode for Lit-enhanced TypeScript with lit-html template support."
  (lit-ts-mode--setup))

(provide 'lit-ts-mode)

;;; lit-ts-mode.el ends here

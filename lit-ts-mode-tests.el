;;; lit-ts-mode-tests.el --- Tests for lit-ts-mode -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'lit-ts-mode)

;;; Mode derivation tests

(ert-deftest lit-ts-mode-test-js-derives-from-js-ts-mode ()
  "lit-ts-js-mode should derive from js-ts-mode."
  (should (eq (get 'lit-ts-js-mode 'derived-mode-parent) 'js-ts-mode)))

(ert-deftest lit-ts-mode-test-ts-derives-from-typescript-ts-mode ()
  "lit-ts-typescript-mode should derive from typescript-ts-mode."
  (should (eq (get 'lit-ts-typescript-mode 'derived-mode-parent)
              'typescript-ts-mode)))

;;; Setup tests

(ert-deftest lit-ts-mode-test-setup-creates-parsers ()
  "Setup should create css and lit-html parsers."
  (with-temp-buffer
    (insert "const x = html`<div class=\"foo\">hello</div>`;\n")
    (lit-ts-js-mode)
    (should (seq-find (lambda (p) (eq (treesit-parser-language p) 'css))
                      (treesit-parser-list)))
    (should (seq-find (lambda (p) (eq (treesit-parser-language p) 'lit-html))
                      (treesit-parser-list)))))

(ert-deftest lit-ts-mode-test-range-settings-configured ()
  "Setup should configure treesit-range-settings."
  (with-temp-buffer
    (insert "const x = html`<div>hello</div>`;\n")
    (lit-ts-js-mode)
    (should treesit-range-settings)))

(ert-deftest lit-ts-mode-test-alist-configured ()
  "Setup should set the tag-grammar alist to use lit-html."
  (with-temp-buffer
    (insert "const x = html`<div>hello</div>`;\n")
    (lit-ts-js-mode)
    (should (equal (assoc "html" template-literals-ts-tag-grammar-alist)
                   '("html" . lit-html)))
    (should (equal (assoc "css" template-literals-ts-tag-grammar-alist)
                   '("css" . css)))))

(ert-deftest lit-ts-mode-test-capf-in-completion-functions ()
  "Setup should add capf to completion-at-point-functions."
  (with-temp-buffer
    (insert "const x = html`<div>hello</div>`;\n")
    (lit-ts-js-mode)
    (should (memq #'lit-ts-mode--capf completion-at-point-functions))))

;;; Candidate collection tests

(ert-deftest lit-ts-mode-test-collect-css-classes ()
  "Should collect class names from CSS selectors."
  (with-temp-buffer
    (insert "const s = css`.foo { color: red; } .bar { display: flex; }`;\n")
    (lit-ts-js-mode)
    (treesit-update-ranges)
    (let ((candidates (lit-ts-mode--collect-css-classes-and-ids)))
      (should (member '("foo" . "class") candidates))
      (should (member '("bar" . "class") candidates)))))

(ert-deftest lit-ts-mode-test-collect-css-ids ()
  "Should collect ID names from CSS selectors."
  (with-temp-buffer
    (insert "const s = css`#main { width: 100%; }`;\n")
    (lit-ts-js-mode)
    (treesit-update-ranges)
    (let ((candidates (lit-ts-mode--collect-css-classes-and-ids)))
      (should (member '("main" . "id") candidates)))))

(ert-deftest lit-ts-mode-test-collect-html-classes ()
  "Should collect class names from HTML class attributes."
  (with-temp-buffer
    (insert "const t = html`<div class=\"alpha beta\">hi</div>`;\n")
    (lit-ts-js-mode)
    (treesit-update-ranges)
    (let ((candidates (lit-ts-mode--collect-html-classes-and-ids)))
      (should (member '("alpha" . "class") candidates))
      (should (member '("beta" . "class") candidates)))))

(ert-deftest lit-ts-mode-test-collect-html-ids ()
  "Should collect ID values from HTML id attributes."
  (with-temp-buffer
    (insert "const t = html`<div id=\"container\">hi</div>`;\n")
    (lit-ts-js-mode)
    (treesit-update-ranges)
    (let ((candidates (lit-ts-mode--collect-html-classes-and-ids)))
      (should (member '("container" . "id") candidates)))))

;;; CAPF context tests

(ert-deftest lit-ts-mode-test-capf-returns-nil-in-host ()
  "CAPF should return nil when point is in host language context."
  (with-temp-buffer
    (insert "const x = html`<div>hi</div>`;\n")
    (lit-ts-js-mode)
    (treesit-update-ranges)
    (goto-char (point-min))
    ;; Point on "const" — host language
    (should-not (lit-ts-mode--capf))))

(ert-deftest lit-ts-mode-test-capf-css-after-dot ()
  "CAPF should offer class candidates in CSS context after dot."
  (with-temp-buffer
    (insert "const s = css`.foo { color: red; } .bar { }`;\n")
    (insert "const t = html`<div class=\"baz\">hi</div>`;\n")
    (lit-ts-js-mode)
    (treesit-update-ranges)
    ;; Move point to right after the "." in ".bar" (inside CSS context)
    (goto-char (point-min))
    (search-forward " .bar")
    (backward-char 3)
    ;; Point is now between . and b in .bar
    (let ((result (lit-ts-mode--capf)))
      (should result)
      (should (member "foo" (nth 2 result))))))

(ert-deftest lit-ts-mode-test-capf-html-class-attr ()
  "CAPF should offer candidates inside HTML class attribute."
  (with-temp-buffer
    (insert "const s = css`.myclass { color: red; }`;\n")
    (insert "const t = html`<div class=\"myclass \">hi</div>`;\n")
    (lit-ts-js-mode)
    (treesit-update-ranges)
    ;; Move point inside the class attribute value, after the space
    (goto-char (point-min))
    (search-forward "class=\"myclass ")
    ;; Point is now right after the space inside class="myclass "
    (let ((result (lit-ts-mode--capf)))
      (should result)
      (should (member "myclass" (nth 2 result))))))

(provide 'lit-ts-mode-tests)

;;; lit-ts-mode-tests.el ends here

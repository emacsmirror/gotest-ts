;;; gotest-ts.el --- Go test runner with tree-sitter support -*- lexical-binding: t -*-

;; Author: Chmouel Boudjnah
;; Maintainer: Chmouel Boudjnah
;; Version: 0.3
;; Package-Requires: ((emacs "29.1") (gotest "0.16.0"))
;; Homepage: https://github.com/chmouel/gotest-ts.el
;; Keywords: languages, go, tests, tree-sitter

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; This package provides enhanced Go test running capabilities using tree-sitter
;; for accurate syntax parsing. It intelligently detects test functions and
;; subtests at point, allowing you to run specific tests with a single command.
;;
;; Features:
;; - Automatic detection of test functions and table-driven subtests
;; - Tree-sitter powered parsing for reliable syntax understanding
;; - DWIM (Do What I Mean) test execution
;; - Support for customizable subtest field names
;; - Navigation between subtests with next/previous commands
;;
;; Usage:
;; Place your cursor on a test function or within a subtest case and run:
;; M-x gotest-ts-run-dwim
;;
;; Navigate between subtests:
;; M-x gotest-ts-next-subtest  ; Go to next subtest
;; M-x gotest-ts-prev-subtest  ; Go to previous subtest
;;
;; For table-driven tests like:
;;   func TestExample(t *testing.T) {
;;       tests := []struct {
;;           name string
;;           input int
;;           want int
;;       }{
;;           {name: "case 1", input: 1, want: 2},
;;           {name: "case 2", input: 2, want: 4},
;;       }
;;       // ...
;;   }
;;
;; The package will automatically detect if you're on a specific test case
;; and run only that subtest, or run the entire function if you're elsewhere.
;;
;; Suggested keybinding:
;; (define-key go-ts-mode-map (kbd "C-c t r") #'gotest-ts-run-dwim)
;; (define-key go-ts-mode-map (kbd "C-c t n") #'gotest-ts-next-subtest)
;; (define-key go-ts-mode-map (kbd "C-c t p") #'gotest-ts-prev-subtest)

;;; Code:

(require 'gotest)
(require 'treesit)

(defgroup gotest-ts nil
  "Go test runner with tree-sitter support."
  :group 'tools
  :group 'go
  :prefix "gotest-ts-")

(defcustom gotest-ts-subtest-field-name "name"
  "The field name used to identify a subtest in table-driven tests.
This is typically 'name' but could be 'description', 'testName', etc."
  :type 'string
  :group 'gotest-ts)

(defcustom gotest-ts-require-go-mode t
  "Whether to require the buffer to be in a Go mode before running tests.
When non-nil, commands will only work in `go-mode' or `go-ts-mode'."
  :type 'boolean
  :group 'gotest-ts)

(defun gotest-ts--validate-environment ()
  "Validate that the current environment supports gotest-ts operations.
Returns non-nil if valid, signals an error otherwise."
  (cond
   ((not (buffer-file-name))
    (user-error "Buffer is not visiting a file"))

   ((not (string-match-p "_test\\.go\\'" (buffer-file-name)))
    (user-error "Current file is not a Go test file (*_test.go)"))

   ((and gotest-ts-require-go-mode
         (not (derived-mode-p 'go-mode 'go-ts-mode)))
    (user-error "Current buffer is not in Go mode"))

   ((not (treesit-available-p))
    (user-error "Tree-sitter is not available in this Emacs build"))

   ((not (treesit-language-available-p 'go))
    (user-error "Go language support is not available for tree-sitter"))

   (t t)))

(defun gotest-ts--get-test-function-name ()
  "Get the name of the test function at point.
Returns nil if not inside a test function."
  (let ((defun-node (treesit-defun-at-point)))
    (when defun-node
      (let* ((name-node (treesit-node-child-by-field-name defun-node "name"))
             (func-name (when name-node
                          (substring-no-properties (treesit-node-text name-node)))))
        (when (and func-name (string-match-p "^Test" func-name))
          func-name)))))

(defun gotest-ts--find-subtest-name ()
  "Find the subtest name at point within a table-driven test.
Returns the subtest name if found, nil otherwise."
  (let ((current-node (treesit-node-at (point)))
        (subtest-name nil))

    ;; Walk up the tree to find all literal_value nodes
    (while (and current-node (not subtest-name))
      (when (string-equal (treesit-node-type current-node) "literal_value")
        ;; Check if this literal_value contains our subtest field name
        (let ((children (treesit-node-children current-node))
              (found-subtest-field nil))

          (dolist (child children)
            (when (string-equal (treesit-node-type child) "keyed_element")
              (let ((child-text (treesit-node-text child)))
                ;; Match various patterns: name: "value", name:"value", name:`value`
                (when (string-match
                       (format "^%s\\s-*:\\s-*[`\"]\\([^`\"]*\\)[`\"]"
                               (regexp-quote gotest-ts-subtest-field-name))
                       child-text)
                  (setq subtest-name
                        (replace-regexp-in-string
                         "\\s-+" "_"  ; Replace spaces with underscores
                         (match-string 1 child-text)))
                  (setq found-subtest-field t)))))

          ;; If we found the subtest field in this literal_value, we're done
          (when found-subtest-field
            (setq current-node nil))))

      ;; Move to parent node if we haven't found the subtest name yet
      (unless subtest-name
        (setq current-node (treesit-node-parent current-node))))

    subtest-name))

(defun gotest-ts--build-test-pattern ()
  "Build a test pattern for the current context.
Returns a test pattern suitable for `go test -run`."
  (let ((func-name (gotest-ts--get-test-function-name))
        (subtest-name (gotest-ts--find-subtest-name)))

    (unless func-name
      (user-error "Not inside a test function"))

    (if subtest-name
        (format "^%s/%s$" func-name (shell-quote-argument subtest-name))
      (format "^%s$" func-name))))

;;;###autoload
(defun gotest-ts-run-dwim ()
  "Run the test function at point or the subtest at point if applicable.
This command intelligently determines what test to run based on the cursor position:
- If inside a subtest case in a table-driven test, runs only that subtest
- If inside a test function but not in a specific subtest, runs the entire function
- Provides helpful error messages for invalid contexts"
  (interactive)

  (gotest-ts--validate-environment)

  (condition-case err
      (let ((test-pattern (gotest-ts--build-test-pattern)))
        (message "Running test: %s" test-pattern)
        (go-test--go-test (concat "-run " test-pattern " .")))

    (error
     (message "Error running test: %s" (error-message-string err)))))

;;;###autoload
(defun gotest-ts-run-function ()
  "Run the entire test function at point, ignoring any subtest context."
  (interactive)

  (gotest-ts--validate-environment)

  (let ((func-name (gotest-ts--get-test-function-name)))
    (unless func-name
      (user-error "Not inside a test function"))

    (let ((test-pattern (format "^%s$" func-name)))
      (message "Running test function: %s" test-pattern)
      (go-test--go-test (concat "-run " test-pattern " .")))))

;;;###autoload
(defun gotest-ts-show-test-info ()
  "Show information about the test context at point.
Displays the test function name and subtest name (if any) in the minibuffer."
  (interactive)

  (condition-case nil
      (gotest-ts--validate-environment)
    (error
     (message "Not in a valid Go test context")
     (return)))

  (let ((func-name (gotest-ts--get-test-function-name))
        (subtest-name (gotest-ts--find-subtest-name)))

    (cond
     ((and func-name subtest-name)
      (message "Test function: %s, Subtest: %s" func-name subtest-name))
     (func-name
      (message "Test function: %s" func-name))
     (t
      (message "Not inside a test function")))))

(defun gotest-ts--find-all-subtests ()
  "Find all subtests in the current test function.
Returns a list of (position . subtest-name) pairs, sorted by position."
  (let ((defun-node (treesit-defun-at-point))
        (subtests '()))

    (when defun-node
      ;; Search for all literal_value nodes within the function
      (treesit-search-subtree
       defun-node
       (lambda (node)
         (when (string-equal (treesit-node-type node) "literal_value")
           (let ((children (treesit-node-children node))
                 (subtest-name nil)
                 (name-node nil))

             ;; Look for the subtest field in this literal_value
             (dolist (child children)
               (when (string-equal (treesit-node-type child) "keyed_element")
                 (let ((child-text (treesit-node-text child)))
                   (when (string-match
                          (format "^%s\\s-*:\\s-*[`\"]\\([^`\"]*\\)[`\"]"
                                  (regexp-quote gotest-ts-subtest-field-name))
                          child-text)
                     (setq subtest-name
                           (replace-regexp-in-string
                            "\\s-+" "_"
                            (match-string 1 child-text)))
                     (setq name-node child)))))

             ;; If we found a subtest, add it to our list
             (when (and subtest-name name-node)
               (push (cons (treesit-node-start name-node) subtest-name) subtests))))
         nil)  ; Continue searching
       t))  ; Include current node

    ;; Sort by position in file
    (sort subtests (lambda (a b) (< (car a) (car b))))))

(defun gotest-ts--current-subtest-index ()
  "Get the index of the current subtest, or nil if not in a subtest."
  (let ((current-pos (point))
        (subtests (gotest-ts--find-all-subtests))
        (index 0)
        (found-index nil))

    (dolist (subtest subtests)
      (when (>= current-pos (car subtest))
        (setq found-index index))
      (setq index (1+ index)))

    found-index))

;;;###autoload
(defun gotest-ts-next-subtest ()
  "Navigate to the next subtest in the current test function."
  (interactive)

  (gotest-ts--validate-environment)

  (unless (gotest-ts--get-test-function-name)
    (user-error "Not inside a test function"))

  (let ((subtests (gotest-ts--find-all-subtests)))
    (unless subtests
      (user-error "No subtests found in current test function"))

    (let ((current-index (gotest-ts--current-subtest-index))
          (next-index nil))

      (cond
       ;; If we're not in any subtest, go to the first one
       ((null current-index)
        (setq next-index 0))
       ;; If we're at the last subtest, wrap to first
       ((>= current-index (1- (length subtests)))
        (setq next-index 0)
        (message "Wrapped to first subtest"))
       ;; Otherwise, go to next subtest
       (t
        (setq next-index (1+ current-index))))

      (let ((next-subtest (nth next-index subtests)))
        (goto-char (car next-subtest))
        (message "Subtest: %s (%d/%d)"
                 (cdr next-subtest)
                 (1+ next-index)
                 (length subtests))))))

;;;###autoload
(defun gotest-ts-prev-subtest ()
  "Navigate to the previous subtest in the current test function."
  (interactive)

  (gotest-ts--validate-environment)

  (unless (gotest-ts--get-test-function-name)
    (user-error "Not inside a test function"))

  (let ((subtests (gotest-ts--find-all-subtests)))
    (unless subtests
      (user-error "No subtests found in current test function"))

    (let ((current-index (gotest-ts--current-subtest-index))
          (prev-index nil))

      (cond
       ;; If we're not in any subtest, go to the last one
       ((null current-index)
        (setq prev-index (1- (length subtests))))
       ;; If we're at the first subtest, wrap to last
       ((<= current-index 0)
        (setq prev-index (1- (length subtests)))
        (message "Wrapped to last subtest"))
       ;; Otherwise, go to previous subtest
       (t
        (setq prev-index (1- current-index))))

      (let ((prev-subtest (nth prev-index subtests)))
        (goto-char (car prev-subtest))
        (message "Subtest: %s (%d/%d)"
                 (cdr prev-subtest)
                 (1+ prev-index)
                 (length subtests))))))

;;;###autoload
(defun gotest-ts-setup-keybindings ()
  "Set up suggested keybindings for gotest-ts in Go modes.
Binds the following keys in `go-mode-map' and `go-ts-mode-map':
- C-c t r: `gotest-ts-run-dwim'
- C-c t f: `gotest-ts-run-function'
- C-c t i: `gotest-ts-show-test-info'
- C-c t n: `gotest-ts-next-subtest'
- C-c t p: `gotest-ts-prev-subtest'"
  (interactive)

  (let ((bindings '(("C-c t r" . gotest-ts-run-dwim)
                    ("C-c t f" . gotest-ts-run-function)
                    ("C-c t i" . gotest-ts-show-test-info)
                    ("C-c t n" . gotest-ts-next-subtest)
                    ("C-c t p" . gotest-ts-prev-subtest))))

    (dolist (binding bindings)
      (when (boundp 'go-mode-map)
        (define-key go-mode-map (kbd (car binding)) (cdr binding)))
      (when (boundp 'go-ts-mode-map)
        (define-key go-ts-mode-map (kbd (car binding)) (cdr binding))))))

(provide 'gotest-ts)

;;; gotest-ts.el ends here

# GoTest Emacs Mode with Treesitter

A comprehensive Emacs package for running Go tests with intelligent Treesitter support, featuring advanced navigation, subtest detection, and seamless integration with your Go development workflow.

## Table of Contents

* [Introduction](#introduction)
* [Features](#features)
* [Installation](#installation)
* [Setup](#setup)
* [Usage](#usage)
* [Commands](#commands)
* [Keybindings](#keybindings)
* [Configuration](#configuration)
* [Integration](#integration)
* [Examples](#examples)
* [Troubleshooting](#troubleshooting)
* [Contributing](#contributing)
* [Authors](#authors)

## Introduction

This Emacs package provides intelligent Go test running capabilities using
Treesitter for accurate syntax parsing. It automatically detects test functions
and table-driven subtests, offering a seamless "Do What I Mean" experience for
Go developers.

Whether you're working with simple test functions or complex table-driven tests
with nested structures, gotest-ts intelligently determines what test to run
based on your cursor position.

![Demo](https://github.com/user-attachments/assets/d61732b0-68a6-4947-b79f-d87adc3a412a)

## Features

### Core Functionality

* Smart Test Detection: Automatically detects test functions and
  table-driven subtests using Treesitter
* DWIM Test Execution: Run the right test based on cursor position -
  function or specific subtest
* Nested Structure Support: Handles complex nested structures in
  table-driven tests
* Tree-sitter Powered: Reliable syntax parsing that understands Go code
  structure

### Navigation & Discovery

* Subtest Navigation: Jump between subtests with `next-subtest` and `prev-subtest`
* Imenu Integration: Quick navigation to any test or subtest via `imenu`
* Test Information: Display current test context and subtest details
* Position Awareness: Always know where you are in your test suite

## Installation

### Prerequisites

You need to have Go mode configured to use Treesitter. See this
[Article](https://robbmann.io/posts/emacs-treesit-auto/) for setup information.

### Via MELPA

```elisp
(use-package gotest-ts
  :ensure t
  :hook (go-ts-mode . gotest-ts-setup))
```

### Manual Installation

Clone the repository and add it to your load-path:

```bash
git clone https://github.com/chmouel/gotest-ts.el.git
```

```elisp
(add-to-list 'load-path "/path/to/gotest-ts.el")
(require 'gotest-ts)
```

## Setup

### Quick Setup (Recommended)

```elisp
(require 'gotest-ts)

;; Complete setup with keybindings and imenu
(add-hook 'go-ts-mode-hook #'gotest-ts-setup)
```

### Manual Setup

```elisp
;; Keybindings only
(add-hook 'go-ts-mode-hook #'gotest-ts-setup-keybindings)

;; Imenu integration only
(add-hook 'go-ts-mode-hook #'gotest-ts-imenu-setup)
```

## Usage

### Basic Workflow

1. **Position your cursor** in a test function or subtest case
2. **Run the test** with `C-c t r` (`gotest-ts-run-dwim`)
3. **Navigate between tests** using `C-c t n/p` or `C-c t m` (imenu)

### Smart Test Detection

The package intelligently determines what to run:

```go
func TestProcessStatus(t *testing.T) {
    tests := []struct {
        name        string
        wantClient  bool
        args        args{
            statusOpts: provider.StatusOpts{
                Status: "in_progress",
            },
        }
    }{
        {
            name: "skip in progress",     // ← Cursor here = runs this subtest
            wantClient: true,
            // ...
        },
    }
    // ← Cursor here = runs entire function
}
```

### Navigation Features

**Sequential Navigation:**

* `gotest-ts-next-subtest` - Jump to next subtest (with wraparound)
* `gotest-ts-prev-subtest` - Jump to previous subtest (with wraparound)

**Overview Navigation:**

* `gotest-ts-imenu-goto` - Open imenu with all tests and subtests
* Standard `imenu` - Lists all tests in format: `TestName::subtest_name`

## Commands

| Command | Description |
|---------|-------------|
| `gotest-ts-run-dwim` | **Main command**: Run test or subtest at point |
| `gotest-ts-run-function` | Run entire test function (ignore subtests) |
| `gotest-ts-next-subtest` | Navigate to next subtest |
| `gotest-ts-prev-subtest` | Navigate to previous subtest |
| `gotest-ts-show-test-info` | Show current test context information |
| `gotest-ts-imenu-goto` | Open imenu for test navigation |
| `gotest-ts-setup` | Complete setup (keybindings + imenu) |
| `gotest-ts-setup-keybindings` | Setup keybindings only |
| `gotest-ts-imenu-setup` | Setup imenu integration only |

## Keybindings

Default keybindings when using `gotest-ts-setup-keybindings`:

| Key | Command | Description |
|-----|---------|-------------|
| `C-c t r` | `gotest-ts-run-dwim` | Run test/subtest at point |
| `C-c t f` | `gotest-ts-run-function` | Run entire test function |
| `C-c t n` | `gotest-ts-next-subtest` | Next subtest |
| `C-c t p` | `gotest-ts-prev-subtest` | Previous subtest |
| `C-c t m` | `gotest-ts-imenu-goto` | Open test navigation menu |
| `C-c t i` | `gotest-ts-show-test-info` | Show test info |

## Configuration

### Customization Options

```elisp
;; Customize the subtest field name (default: "name")
(setq gotest-ts-subtest-field-name "description")

;; Disable Go mode requirement (default: t)
(setq gotest-ts-require-go-mode nil)
```

### Custom Subtest Fields

The package supports any subtest field name:

```go
// With default "name" field
{name: "test case", input: 1, want: 2}

// With custom "description" field
{description: "test case", input: 1, want: 2}

// Configure: (setq gotest-ts-subtest-field-name "description")
```

## Integration

### With Completion Frameworks

**Ivy/Counsel:**

```elisp
(global-set-key (kbd "C-c t m") #'counsel-imenu)
```

**Helm:**

```elisp
(global-set-key (kbd "C-c t m") #'helm-imenu)
```

**Vertico:**
Works automatically with standard `imenu`.

### With Imenu-List (Sidebar)

```elisp
(use-package imenu-list
  :config
  (add-hook 'go-ts-mode-hook #'gotest-ts-imenu-setup))

;; Toggle sidebar: M-x imenu-list-smart-toggle
```

### Debugging with Dape

Enhanced Dape integration with the new function name:

```elisp
(defun my-dape-go-test-at-point ()
  (interactive)
  (dape (dape--config-eval-1
         `(modes (go-ts-mode)
                 ensure dape-ensure-command
                 fn dape-config-autoport
                 command "dlv"
                 command-args ("dap" "--listen" "127.0.0.1::autoport")
                 command-cwd dape-cwd-fn
                 port :autoport
                 :type "debug"
                 :request "launch"
                 :mode "test"
                 :cwd dape-cwd-fn
                 :program (lambda () (concat "./" (file-relative-name default-directory (funcall dape-cwd-fn))))
                 :args (lambda ()
                         (when-let* ((test-name (gotest-ts--build-test-pattern)))
                           (if test-name `["-test.run" ,test-name]
                             (error "No test selected"))))))))
```

### Imenu Navigation Example

After setup, `M-x imenu` shows:

```
TestProcessData
TestProcessData::valid_input
TestProcessData::invalid_input_with_spaces
TestComplexScenario
TestComplexScenario::complex_case
TestAnotherFunction
```

## Troubleshooting

### Common Issues

**"Tree-sitter is not available"**

* Ensure you're using Emacs 29+ with tree-sitter support
* Install Go language support: `M-x treesit-install-language-grammar RET go`

**"Not inside a test function"**

* Ensure your function name starts with `Test`
* Check you're in a Go test file (`*_test.go`)

**Subtest not detected**

* Verify your field name matches `gotest-ts-subtest-field-name`
* Ensure you're positioned within the correct struct literal

**Imenu not working**

* Run `M-x gotest-ts-imenu-setup` manually
* Check that imenu is properly configured in your Emacs

### Debug Information

Use `gotest-ts-show-test-info` to see what the package detects:

* Shows current test function
* Displays subtest name if detected
* Indicates if you're not in a valid context

## Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Submit a pull request

### Bug Reports

When reporting bugs, please include:

* Emacs version and tree-sitter availability
* Go mode configuration
* Sample Go test code that reproduces the issue
* Error messages or unexpected behavior

## Thanks

This package builds upon the excellent
[gotest](https://github.com/nlamirault/gotest.el) package, adding intelligent
tree-sitter integration and enhanced navigation capabilities.

## Copyright

[GPL-3.0-OR-LATER](./LICENSE)

## Authors

### Chmouel Boudjnah

* Fediverse - [@chmouel@chmouel.com](https://fosstodon.org/@chmouel)
* Twitter - [@chmouel](https://twitter.com/chmouel)
* Blog - [https://blog.chmouel.com](https://blog.chmouel.com)

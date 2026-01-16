# `use-package-treesit`

Automatically install and enable Tree-sitter grammars when using use-package.

This package adds a new `:treesit` keyword to use-package
declarations. When present, the corresponding Tree-sitter grammar is
automatically downloaded and compiled the first time the package (or
its major mode) is actually used.

Works with Emacs 30+, using the built-in Tree-sitter (treesit) support.

## Features

- Adds `:treesit` keyword to `use-package`
- Lazy grammar installation (only when needed)
- Uses Emacs’ native mechanisms, i.e. `treesit-install-language-grammar`
- Built-in recipe list for common languages

## Requirements

- Emacs 30.0 or newer (native Tree-sitter support)
- `use-package`

## Usage

```elisp
(use-package use-package-treesit :ensure)

(use-package dockerfile-ts-mode
  :treesit
  :mode "Dockerfile")
```

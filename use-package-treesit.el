;;; use-package-treesit.el --- Automatically use tree-sitter enhanced major modes -*- lexical-binding: t -*-

;; Copyright (C) 2023 Robert Enzmann
;; Copyright (C) 2026 École Polytechnique Fédérale de Lausanne (EPFL)

;; Author: Dominique Quatravaux <dominique@quatravaux.org>
;; Keywords: tree-sitter treesit auto automatic use-package
;; URL: https://github.com/domq/use-package-treesit.git
;; Package-Requires: ((emacs "30.0"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Provides the `(use-package foo :treesit)' idiom.
;;
;; Adding the `:treesit' keyword to your package's `use-package'
;; stanza (otherwise similar to any other `use-package' invocation),
;; causes the corresponding treesit grammar to be downloaded and
;; installed automatically the first time the package loads (which may
;; mean immediately, unless the `use-package' configuration uses
;; `:defer', `:mode', `:commands' or any of the other keywords that
;; cause deferred loading).
;;
;; The `:treesit' keyword may take additional arguments in the future
;; (to be specified and implemented later). The default (and right
;; now, only) behavior is to download and compile the tree-sitter
;; grammar out of a location taken from the
;; `use-package-treesit-recipes`, using Emacs 30+'s built-in
;; mechanisms. Specifically,
;;
;; - at the time the `use-package' stanza is evaluated, and provided
;;   the target package is available (or equivalently, in the same
;;   time frame ad under the same preconditions as use-pacakge's
;;   `:init' clause): a new is entry is added to
;;   `treesit-language-source-alist' for the target language (unless
;;   one already exists),
;;
;; - before the target package actually loads:
;;   `treesit-install-language-grammar' is invoked.

;;; Code:

(require 'treesit)
(require 'use-package-core)
(eval-when-compile
  (require 'cl-lib)
  (require 'cl-macs)
  (require 'gv))

(cl-defstruct use-package-treesit-recipe
  "Emacs metadata for a treesit language grammar."
  lang mode url revision source-dir cc c++)

(defvar use-package-treesit-recipes
  `(,(make-use-package-treesit-recipe
      :lang 'awk
      :mode 'awk-ts-mode
      :url "https://github.com/Beaglefoot/tree-sitter-awk")
    ,(make-use-package-treesit-recipe
      :lang 'bash
      :mode 'bash-ts-mode
      :url "https://github.com/tree-sitter/tree-sitter-bash")
    ,(make-use-package-treesit-recipe
      :lang 'bibtex
      :mode 'bibtex-ts-mode
      :url "https://github.com/latex-lsp/tree-sitter-bibtex")
    ,(make-use-package-treesit-recipe
      :lang 'blueprint
      :mode 'blueprint-ts-mode
      :url "https://github.com/huanie/tree-sitter-blueprint")
    ,(make-use-package-treesit-recipe
      :lang 'c
      :mode 'c-ts-mode
      :url "https://github.com/tree-sitter/tree-sitter-c")
    ,(make-use-package-treesit-recipe
      :lang 'c-sharp
      :mode 'csharp-ts-mode
      :url "https://github.com/tree-sitter/tree-sitter-c-sharp")
    ,(make-use-package-treesit-recipe
      :lang 'clojure
      :mode 'clojure-ts-mode
      :url "https://github.com/sogaiu/tree-sitter-clojure")
    ,(make-use-package-treesit-recipe
      :lang 'cmake
      :mode 'cmake-ts-mode
      :url "https://github.com/uyha/tree-sitter-cmake")
    ,(make-use-package-treesit-recipe
      :lang 'commonlisp
      :mode 'commonlisp-ts-mode
      :url "https://github.com/tree-sitter-grammars/tree-sitter-commonlisp")
    ,(make-use-package-treesit-recipe
      :lang 'cpp
      :mode 'c++-ts-mode
      :url "https://github.com/tree-sitter/tree-sitter-cpp")
    ,(make-use-package-treesit-recipe
      :lang 'css
      :mode 'css-ts-mode
      :url "https://github.com/tree-sitter/tree-sitter-css")
    ,(make-use-package-treesit-recipe
      :lang 'dart
      :mode 'dart-ts-mode
      :url "https://github.com/ast-grep/tree-sitter-dart")
    ,(make-use-package-treesit-recipe
      :lang 'dockerfile
      :mode 'dockerfile-ts-mode
      :url "https://github.com/camdencheek/tree-sitter-dockerfile")
    ,(make-use-package-treesit-recipe
      :lang 'elixir
      :mode 'elixir-ts-mode
      :url "https://github.com/elixir-lang/tree-sitter-elixir")
    ,(make-use-package-treesit-recipe
      :lang 'glsl
      :mode 'glsl-ts-mode
      :url "https://github.com/tree-sitter-grammars/tree-sitter-glsl")
    ,(make-use-package-treesit-recipe
      :lang 'go
      :mode 'go-ts-mode
      :url "https://github.com/tree-sitter/tree-sitter-go")
    ,(make-use-package-treesit-recipe
      :lang 'gomod
      :mode 'go-mod-ts-mode
      :url "https://github.com/camdencheek/tree-sitter-go-mod")
    ,(make-use-package-treesit-recipe
      :lang 'heex
      :mode 'heex-ts-mode
      :url "https://github.com/phoenixframework/tree-sitter-heex")
    ,(make-use-package-treesit-recipe
      :lang 'html
      :mode 'html-ts-mode
      :url "https://github.com/tree-sitter/tree-sitter-html")
    ,(make-use-package-treesit-recipe
      :lang 'janet
      :mode 'janet-ts-mode
      :url "https://github.com/sogaiu/tree-sitter-janet-simple")
    ,(make-use-package-treesit-recipe
      :lang 'java
      :mode 'java-ts-mode
      :url "https://github.com/tree-sitter/tree-sitter-java")
    ,(make-use-package-treesit-recipe
      :lang 'javascript
      :mode 'js-ts-mode
      :url "https://github.com/tree-sitter/tree-sitter-javascript"
      :revision "master"
      :source-dir "src")
    ,(make-use-package-treesit-recipe
      :lang 'json
      :mode 'json-ts-mode
      :url "https://github.com/tree-sitter/tree-sitter-json")
    ,(make-use-package-treesit-recipe
      :lang 'julia
      :mode 'julia-ts-mode
      :url "https://github.com/tree-sitter/tree-sitter-julia")
    ,(make-use-package-treesit-recipe
      :lang 'kotlin
      :mode 'kotlin-ts-mode
      :url "https://github.com/fwcd/tree-sitter-kotlin")
    ,(make-use-package-treesit-recipe
      :lang 'latex
      :mode 'latex-ts-mode
      :url "https://github.com/latex-lsp/tree-sitter-latex")
    ,(make-use-package-treesit-recipe
      :lang 'lua
      :mode 'lua-ts-mode
      :url "https://github.com/tree-sitter-grammars/tree-sitter-lua")
    ,(make-use-package-treesit-recipe
      :lang 'magik
      :mode 'magik-ts-mode
      :url "https://github.com/krn-robin/tree-sitter-magik")
    ,(make-use-package-treesit-recipe
      :lang 'make
      :mode 'makefile-ts-mode
      :url "https://github.com/tree-sitter-grammars/tree-sitter-make")
    ,(make-use-package-treesit-recipe
      :lang 'markdown
      :mode 'markdown-ts-mode
      :url "https://github.com/tree-sitter-grammars/tree-sitter-markdown")
    ,(make-use-package-treesit-recipe
      :lang 'nix
      :mode 'nix-ts-mode
      :url "https://github.com/nix-community/tree-sitter-nix")
    ,(make-use-package-treesit-recipe
      :lang 'nu
      :mode 'nushell-ts-mode
      :url "https://github.com/nushell/tree-sitter-nu")
    ,(make-use-package-treesit-recipe
      :lang 'org
      :mode 'org-ts-mode
      :url "https://github.com/milisims/tree-sitter-org")
    ,(make-use-package-treesit-recipe
      :lang 'perl
      :mode 'perl-ts-mode
      :url "https://github.com/ganezdragon/tree-sitter-perl")
    ,(make-use-package-treesit-recipe
      :lang 'proto
      :mode 'protobuf-ts-mode
      :url "https://github.com/mitchellh/tree-sitter-proto")
    ,(make-use-package-treesit-recipe
      :lang 'python
      :mode 'python-ts-mode
      :url "https://github.com/tree-sitter/tree-sitter-python")
    ,(make-use-package-treesit-recipe
      :lang 'r
      :mode 'r-ts-mode
      :url "https://github.com/r-lib/tree-sitter-r")
    ,(make-use-package-treesit-recipe
      :lang 'ruby
      :mode 'ruby-ts-mode
      :url "https://github.com/tree-sitter/tree-sitter-ruby")
    ,(make-use-package-treesit-recipe
      :lang 'rust
      :mode 'rust-ts-mode
      :url "https://github.com/tree-sitter/tree-sitter-rust")
    ,(make-use-package-treesit-recipe
      :lang 'scala
      :mode 'scala-ts-mode
      :url "https://github.com/tree-sitter/tree-sitter-scala")
    ,(make-use-package-treesit-recipe
      :lang 'sql
      :mode 'sql-ts-mode
      :revision "gh-pages"
      :url "https://github.com/DerekStride/tree-sitter-sql")
    ,(make-use-package-treesit-recipe
      :lang 'surface
      :mode 'surface-ts-mode
      :url "https://github.com/connorlay/tree-sitter-surface")
    ,(make-use-package-treesit-recipe
      :lang 'toml
      :mode 'toml-ts-mode
      :url "https://github.com/tree-sitter/tree-sitter-toml")
    ,(make-use-package-treesit-recipe
      :lang 'tsx
      :mode 'tsx-ts-mode
      :url "https://github.com/tree-sitter/tree-sitter-typescript"
      :revision "master"
      :source-dir "tsx/src")
    ,(make-use-package-treesit-recipe
      :lang 'typescript
      :mode 'typescript-ts-mode
      :url "https://github.com/tree-sitter/tree-sitter-typescript"
      :revision "master"
      :source-dir "typescript/src")
    ,(make-use-package-treesit-recipe
      :lang 'typst
      :mode 'typst-ts-mode
      :url "https://github.com/uben0/tree-sitter-typst"
      :revision "master"
      :source-dir "src")
    ,(make-use-package-treesit-recipe
      :lang 'verilog
      :mode 'verilog-ts-mode
      :url "https://github.com/gmlarumbe/tree-sitter-verilog")
    ,(make-use-package-treesit-recipe
      :lang 'vhdl
      :mode 'vhdl-ts-mode
      :url "https://github.com/alemuller/tree-sitter-vhdl")
    ,(make-use-package-treesit-recipe
      :lang 'vue
      :mode 'vue-ts-mode
      :url "https://github.com/tree-sitter-grammars/tree-sitter-vue")
    ,(make-use-package-treesit-recipe
      :lang 'wast
      :mode 'wat-ts-wast-mode
      :url "https://github.com/wasm-lsp/tree-sitter-wasm"
      :source-dir "wast/src")
    ,(make-use-package-treesit-recipe
      :lang 'wat
      :mode 'wat-ts-mode
      :url "https://github.com/wasm-lsp/tree-sitter-wasm"
      :source-dir "wat/src")
    ,(make-use-package-treesit-recipe
      :lang 'wgsl
      :mode 'wgsl-ts-mode
      :url "https://github.com/mehmetoguzderin/tree-sitter-wgsl")
    ,(make-use-package-treesit-recipe
      :lang 'yaml
      :mode 'yaml-ts-mode
      :url "https://github.com/tree-sitter-grammars/tree-sitter-yaml"))
  "All the treesit languages that `use-package-treesit' can install automatically.")

(defvar use-package-treesit-keyword :treesit)

(defun use-package-normalize/:treesit (name-symbol _keyword _args)
  (list name-symbol))

(defun use-package-handler/:treesit (name-symbol _keyword _args rest state)
  (let ((body (use-package-process-keywords name-symbol rest state)))
    (use-package-concat
     body
     `((use-package-treesit/prepare-auto-install ',name-symbol)))))

(defun use-package-treesit/recipe-of-mode (mode)
  "Find a match for MODE in the variable `use-package-treesit-recipes'."
  (cl-find-if (lambda (it) (eq (use-package-treesit-recipe-mode it) mode))
              use-package-treesit-recipes))

(defun use-package-treesit/prepare-auto-install (mode)
  "Arrange for MODE's treesit grammar to be lazily installed.

Add the match for MODE in `use-package-treesit-recipes' into the
variable `treesit-language-source-alist', where
`use-package-treesit/maybe-install-lazy' will pick it up."
  (when-let ((r (use-package-treesit/recipe-of-mode mode)))
    (setf (alist-get (use-package-treesit-recipe-lang r)
                     treesit-language-source-alist)
          (list (use-package-treesit-recipe-url r)
                (use-package-treesit-recipe-revision r)
                (use-package-treesit-recipe-source-dir r)
                (use-package-treesit-recipe-cc r)
                (use-package-treesit-recipe-c++ r)))))

(defun use-package-treesit/maybe-install-lazy (language &rest _ignored)
  "If so configured, install LANGUAGE just before it will be required.

This function is used as before advice on core Emacs `treesit-ready-p'
and `treesit-parser-create' functions."
  (cond ((treesit-language-available-p language))
        ((assoc language treesit-language-source-alist)
         (progn
           (message "Installing the treesit grammar for %s" language)
           (treesit-install-language-grammar language)))))

(defun use-package-treesit/configure ()
  "Configure `use-package' for the `:treesit' keyword.

Insert the `:treesit' keyword (or whatever the value of the variable
`use-package-treesit-keyword' is) into the variable
`use-package-keywords' at the proper place, unless it is already there.
Place before-advice on `treesit-ready-p' and `treesit-parser-create'
functions, so as to install the grammars before running them."
  (unless (member use-package-treesit-keyword use-package-keywords)
    (setq use-package-keywords
          (let* ((before-pos (cl-position :custom use-package-keywords))
                 (head (cl-subseq use-package-keywords 0 before-pos))
                 (tail (nthcdr before-pos use-package-keywords)))
            (append head `(,use-package-treesit-keyword) tail))))
  (advice-add 'treesit-ready-p :before #'use-package-treesit/maybe-install-lazy)
  (advice-add 'treesit-parser-create :before #'use-package-treesit/maybe-install-lazy))

(use-package-treesit/configure)

(provide 'use-package-treesit)
;;; use-package-treesit.el ends here

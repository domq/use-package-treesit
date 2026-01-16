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

(defvar use-package-treesit-recipes
  '((:lang awk
     :mode awk-ts-mode
     :url "https://github.com/Beaglefoot/tree-sitter-awk")
    (:lang bash
     :mode bash-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-bash")
    (:lang bibtex
     :mode bibtex-ts-mode
     :url "https://github.com/latex-lsp/tree-sitter-bibtex")
    (:lang blueprint
     :mode blueprint-ts-mode
     :url "https://github.com/huanie/tree-sitter-blueprint")
    (:lang c
     :mode c-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-c")
    (:lang c-sharp
     :mode csharp-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-c-sharp")
    (:lang clojure
     :mode clojure-ts-mode
     :url "https://github.com/sogaiu/tree-sitter-clojure")
    (:lang cmake
     :mode cmake-ts-mode
     :url "https://github.com/uyha/tree-sitter-cmake")
    (:lang commonlisp
     :mode commonlisp-ts-mode
     :url "https://github.com/tree-sitter-grammars/tree-sitter-commonlisp")
    (:lang cpp
     :mode c++-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-cpp")
    (:lang css
     :mode css-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-css")
    (:lang dart
     :mode dart-ts-mode
     :url "https://github.com/ast-grep/tree-sitter-dart")
    (:lang dockerfile
     :mode dockerfile-ts-mode
     :url "https://github.com/camdencheek/tree-sitter-dockerfile")
    (:lang elixir
     :mode elixir-ts-mode
     :url "https://github.com/elixir-lang/tree-sitter-elixir")
    (:lang glsl
     :mode glsl-ts-mode
     :url "https://github.com/tree-sitter-grammars/tree-sitter-glsl")
    (:lang go
     :mode go-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-go")
    (:lang gomod
     :mode go-mod-ts-mode
     :url "https://github.com/camdencheek/tree-sitter-go-mod")
    (:lang heex
     :mode heex-ts-mode
     :url "https://github.com/phoenixframework/tree-sitter-heex")
    (:lang html
     :mode html-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-html")
    (:lang janet
     :mode janet-ts-mode
     :url "https://github.com/sogaiu/tree-sitter-janet-simple")
    (:lang java
     :mode java-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-java")
    (:lang javascript
     :mode js-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-javascript"
     :revision "master"
     :source-dir "src")
    (:lang json
     :mode json-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-json")
    (:lang julia
     :mode julia-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-julia")
    (:lang kotlin
     :mode kotlin-ts-mode
     :url "https://github.com/fwcd/tree-sitter-kotlin")
    (:lang latex
     :mode latex-ts-mode
     :url "https://github.com/latex-lsp/tree-sitter-latex")
    (:lang lua
     :mode lua-ts-mode
     :url "https://github.com/tree-sitter-grammars/tree-sitter-lua")
    (:lang magik
     :mode magik-ts-mode
     :url "https://github.com/krn-robin/tree-sitter-magik")
    (:lang make
     :mode makefile-ts-mode
     :url "https://github.com/tree-sitter-grammars/tree-sitter-make")
    (:lang markdown
     :mode markdown-ts-mode
     :url "https://github.com/tree-sitter-grammars/tree-sitter-markdown")
    (:lang nix
     :mode nix-ts-mode
     :url "https://github.com/nix-community/tree-sitter-nix")
    (:lang nu
     :mode nushell-ts-mode
     :url "https://github.com/nushell/tree-sitter-nu")
    (:lang org
     :mode org-ts-mode
     :url "https://github.com/milisims/tree-sitter-org")
    (:lang perl
     :mode perl-ts-mode
     :url "https://github.com/ganezdragon/tree-sitter-perl")
    (:lang proto
     :mode protobuf-ts-mode
     :url "https://github.com/mitchellh/tree-sitter-proto")
    (:lang python
     :mode python-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-python")
    (:lang r
     :mode r-ts-mode
     :url "https://github.com/r-lib/tree-sitter-r")
    (:lang ruby
     :mode ruby-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-ruby")
    (:lang rust
     :mode rust-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-rust")
    (:lang scala
     :mode scala-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-scala")
    (:lang sql
     :mode sql-ts-mode
     :revision "gh-pages"
     :url "https://github.com/DerekStride/tree-sitter-sql")
    (:lang surface
     :mode surface-ts-mode
     :url "https://github.com/connorlay/tree-sitter-surface")
    (:lang toml
     :mode toml-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-toml")
    (:lang tsx
     :mode tsx-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-typescript"
     :revision "master"
     :source-dir "tsx/src")
    (:lang typescript
     :mode typescript-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-typescript"
     :revision "master"
     :source-dir "typescript/src")
    (:lang typst
     :mode typst-ts-mode
     :url "https://github.com/uben0/tree-sitter-typst"
     :revision "master"
     :source-dir "src")
    (:lang verilog
     :mode verilog-ts-mode
     :url "https://github.com/gmlarumbe/tree-sitter-verilog")
    (:lang vhdl
     :mode vhdl-ts-mode
     :url "https://github.com/alemuller/tree-sitter-vhdl")
    (:lang vue
     :mode vue-ts-mode
     :url "https://github.com/tree-sitter-grammars/tree-sitter-vue")
    (:lang wast
     :mode wat-ts-wast-mode
     :url "https://github.com/wasm-lsp/tree-sitter-wasm"
     :source-dir "wast/src")
    (:lang wat
     :mode wat-ts-mode
     :url "https://github.com/wasm-lsp/tree-sitter-wasm"
     :source-dir "wat/src")
    (:lang wgsl
     :mode wgsl-ts-mode
     :url "https://github.com/mehmetoguzderin/tree-sitter-wgsl")
    (:lang yaml
     :mode yaml-ts-mode
     :url "https://github.com/tree-sitter-grammars/tree-sitter-yaml"))
  "All the treesit languages that `use-package-treesit' can install automatically.")

(defun use-package-treesit/recipe-of-mode (mode)
  "Find a match for MODE in the variable `use-package-treesit-recipes'."
  (cl-find-if (lambda (it) (eq (plist-get it :mode) mode))
              use-package-treesit-recipes))

(defvar use-package-treesit-keyword :treesit)

(defun use-package-normalize/:treesit (name-symbol _keyword _args)
  (list name-symbol))

(defun use-package-handler/:treesit (name-symbol _keyword _args rest state)
  (let ((body (use-package-process-keywords name-symbol rest state)))
    (use-package-concat
     body
     `((use-package-treesit/prepare-auto-install ',name-symbol)))))

(defun use-package-treesit/prepare-auto-install (mode)
  "Arrange for MODE's treesit grammar to be lazily installed.

Add the match for MODE in `use-package-treesit-recipes' into the
variable `treesit-language-source-alist', where
`use-package-treesit/maybe-install-lazy' will pick it up."
  (when-let ((r (use-package-treesit/recipe-of-mode mode)))
    (setf (alist-get (plist-get r :lang) treesit-language-source-alist)
          (mapcar (lambda (c) (plist-get r c))
                  '(:url :revision :source-dir :cc :c++)))))

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

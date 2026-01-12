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
  (require 'cl-lib))

(cl-defstruct use-package-treesit-recipe
  "Emacs metadata for a treesit language grammar."
  lang ts-mode remap requires url revision source-dir cc c++ ext)

(defvar use-package-treesit-recipes
  `(,(make-use-package-treesit-recipe
      :lang 'awk
      :ts-mode 'awk-ts-mode
      :remap 'awk-mode
      :url "https://github.com/Beaglefoot/tree-sitter-awk"
      :ext "\\.awk\\'")
    ,(make-use-package-treesit-recipe
      :lang 'bash
      :ts-mode 'bash-ts-mode
      :remap 'sh-mode
      :url "https://github.com/tree-sitter/tree-sitter-bash"
      :ext "\\.sh\\'")
    ,(make-use-package-treesit-recipe
      :lang 'bibtex
      :ts-mode 'bibtex-ts-mode
      :remap 'bibtex-mode
      :url "https://github.com/latex-lsp/tree-sitter-bibtex"
      :ext "\\.bib\\'")
    ,(make-use-package-treesit-recipe
      :lang 'blueprint
      :ts-mode 'blueprint-ts-mode
      :remap 'blueprint-mode
      :url "https://github.com/huanie/tree-sitter-blueprint"
      :ext "\\.blp\\'")
    ,(make-use-package-treesit-recipe
      :lang 'c
      :ts-mode 'c-ts-mode
      :remap 'c-mode
      :url "https://github.com/tree-sitter/tree-sitter-c"
      :ext "\\.c\\'")
    ,(make-use-package-treesit-recipe
      :lang 'c-sharp
      :ts-mode 'csharp-ts-mode
      :remap 'csharp-mode
      :url "https://github.com/tree-sitter/tree-sitter-c-sharp"
      :ext "\\.cs\\'")
    ,(make-use-package-treesit-recipe
      :lang 'clojure
      :ts-mode 'clojure-ts-mode
      :remap '(clojure-mode clojurescript-mode clojurec-mode)
      :url "https://github.com/sogaiu/tree-sitter-clojure"
      :ext "\\.cljc?s?d?\\'")
    ,(make-use-package-treesit-recipe
      :lang 'cmake
      :ts-mode 'cmake-ts-mode
      :remap 'cmake-mode
      :url "https://github.com/uyha/tree-sitter-cmake"
      :ext "\\.cmake\\'")
    ,(make-use-package-treesit-recipe
      :lang 'commonlisp
      :ts-mode 'commonlisp-ts-mode
      :remap 'common-lisp-mode
      :url "https://github.com/tree-sitter-grammars/tree-sitter-commonlisp"
      :ext "\\.cl\\'")
    ,(make-use-package-treesit-recipe
      :lang 'cpp
      :ts-mode 'c++-ts-mode
      :remap 'c++-mode
      :url "https://github.com/tree-sitter/tree-sitter-cpp"
      :ext "\\.cpp\\'")
    ,(make-use-package-treesit-recipe
      :lang 'css
      :ts-mode 'css-ts-mode
      :remap 'css-mode
      :url "https://github.com/tree-sitter/tree-sitter-css"
      :ext "\\.css\\'")
    ,(make-use-package-treesit-recipe
      :lang 'dart
      :ts-mode 'dart-ts-mode
      :remap 'dart-mode
      :url "https://github.com/ast-grep/tree-sitter-dart"
      :ext "\\.dart\\'")
    ,(make-use-package-treesit-recipe
      :lang 'dockerfile
      :ts-mode 'dockerfile-ts-mode
      :remap 'dockerfile-mode
      :url "https://github.com/camdencheek/tree-sitter-dockerfile"
      :ext "[/\\]\\(?:Containerfile\\|Dockerfile\\)\\(?:\\.[^/\\]*\\)?\\'")
    ,(make-use-package-treesit-recipe
      :lang 'elixir
      :ts-mode 'elixir-ts-mode
      :remap 'elixir-mode
      :requires 'heex
      :url "https://github.com/elixir-lang/tree-sitter-elixir"
      :ext "\\.ex\\'")
    ,(make-use-package-treesit-recipe
      :lang 'glsl
      :ts-mode 'glsl-ts-mode
      :remap 'glsl-mode
      :url "https://github.com/tree-sitter-grammars/tree-sitter-glsl")
    ,(make-use-package-treesit-recipe
      :lang 'go
      :ts-mode 'go-ts-mode
      :remap 'go-mode
      :requires 'gomod
      :url "https://github.com/tree-sitter/tree-sitter-go"
      :ext "\\.go\\'")
    ,(make-use-package-treesit-recipe
      :lang 'gomod
      :ts-mode 'go-mod-ts-mode
      :remap 'go-mod-mode
      :requires 'go
      :url "https://github.com/camdencheek/tree-sitter-go-mod"
      :ext "go\\.mod\\'")
    ,(make-use-package-treesit-recipe
      :lang 'heex
      :ts-mode 'heex-ts-mode
      :remap 'heex-mode
      :url "https://github.com/phoenixframework/tree-sitter-heex"
      :ext "\\.heex\\'")
    ,(make-use-package-treesit-recipe
      :lang 'html
      :ts-mode 'html-ts-mode
      :remap '(mhtml-mode sgml-mode)
      :url "https://github.com/tree-sitter/tree-sitter-html"
      :ext "\\.html\\'")
    ,(make-use-package-treesit-recipe
      :lang 'janet
      :ts-mode 'janet-ts-mode
      :remap 'janet-mode
      :url "https://github.com/sogaiu/tree-sitter-janet-simple"
      :ext "\\.janet\\'")
    ,(make-use-package-treesit-recipe
      :lang 'java
      :ts-mode 'java-ts-mode
      :remap 'java-mode
      :url "https://github.com/tree-sitter/tree-sitter-java"
      :ext "\\.java\\'")
    ,(make-use-package-treesit-recipe
      :lang 'javascript
      :ts-mode 'js-ts-mode
      :remap '(js-mode javascript-mode js2-mode)
      :url "https://github.com/tree-sitter/tree-sitter-javascript"
      :revision "master"
      :source-dir "src"
      :ext "\\.js\\'")
    ,(make-use-package-treesit-recipe
      :lang 'json
      :ts-mode 'json-ts-mode
      :remap 'js-json-mode
      :url "https://github.com/tree-sitter/tree-sitter-json"
      :ext "\\.json\\'")
    ,(make-use-package-treesit-recipe
      :lang 'julia
      :ts-mode 'julia-ts-mode
      :remap 'julia-mode
      :url "https://github.com/tree-sitter/tree-sitter-julia"
      :ext "\\.jl\\'")
    ,(make-use-package-treesit-recipe
      :lang 'kotlin
      :ts-mode 'kotlin-ts-mode
      :remap 'kotlin-mode
      :url "https://github.com/fwcd/tree-sitter-kotlin"
      :ext "\\.kts?\\'")
    ,(make-use-package-treesit-recipe
      :lang 'latex
      :ts-mode 'latex-ts-mode
      :remap 'latex-mode
      :url "https://github.com/latex-lsp/tree-sitter-latex"
      :ext "\\.tex\\'")
    ,(make-use-package-treesit-recipe
      :lang 'lua
      :ts-mode 'lua-ts-mode
      :remap 'lua-mode
      :url "https://github.com/tree-sitter-grammars/tree-sitter-lua"
      :ext "\\.lua\\'")
    ,(make-use-package-treesit-recipe
      :lang 'magik
      :ts-mode 'magik-ts-mode
      :remap 'magik-mode
      :url "https://github.com/krn-robin/tree-sitter-magik"
      :ext "\\.magik\\'")
    ,(make-use-package-treesit-recipe
      :lang 'make
      :ts-mode 'makefile-ts-mode
      :remap 'makefile-mode
      :url "https://github.com/tree-sitter-grammars/tree-sitter-make"
      :ext "\\([Mm]akefile\\|.*\\.\\(mk\\|make\\)\\)\\'")
    ,(make-use-package-treesit-recipe
      :lang 'markdown
      :ts-mode 'markdown-ts-mode
      :remap '(poly-markdown-mode markdown-mode)
      :url "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
      :ext "\\.md\\'")
    ,(make-use-package-treesit-recipe
      :lang 'nix
      :ts-mode 'nix-ts-mode
      :remap 'nix-mode
      :url "https://github.com/nix-community/tree-sitter-nix"
      :ext "\\.nix\\'")
    ,(make-use-package-treesit-recipe
      :lang 'nu
      :ts-mode 'nushell-ts-mode
      :remap 'nushell-mode
      :url "https://github.com/nushell/tree-sitter-nu"
      :ext "\\.nu\\'")
    ,(make-use-package-treesit-recipe
      :lang 'org
      :ts-mode 'org-ts-mode
      :remap 'org-mode
      :url "https://github.com/milisims/tree-sitter-org"
      :ext "\\.org\\'")
    ,(make-use-package-treesit-recipe
      :lang 'perl
      :ts-mode 'perl-ts-mode
      :remap 'perl-mode
      :url "https://github.com/ganezdragon/tree-sitter-perl"
      :ext "\\.pl\\'")
    ,(make-use-package-treesit-recipe
      :lang 'proto
      :ts-mode 'protobuf-ts-mode
      :remap 'protobuf-mode
      :url "https://github.com/mitchellh/tree-sitter-proto"
      :ext "\\.proto\\'")
    ,(make-use-package-treesit-recipe
      :lang 'python
      :ts-mode 'python-ts-mode
      :remap 'python-mode
      :url "https://github.com/tree-sitter/tree-sitter-python"
      :ext "\\.py[iw]?\\'")
    ,(make-use-package-treesit-recipe
      :lang 'r
      :ts-mode 'r-ts-mode
      :remap 'ess-mode
      :url "https://github.com/r-lib/tree-sitter-r"
      :ext "\\.r\\'")
    ,(make-use-package-treesit-recipe
      :lang 'ruby
      :ts-mode 'ruby-ts-mode
      :remap 'ruby-mode
      :url "https://github.com/tree-sitter/tree-sitter-ruby"
      :ext "\\(?:\\.\\(?:rbw?\\|ru\\|rake\\|thor\\|jbuilder\\|rabl\\|gemspec\\|podspec\\)\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Puppet\\|Berks\\|Brew\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'")
    ,(make-use-package-treesit-recipe
      :lang 'rust
      :ts-mode 'rust-ts-mode
      :remap 'rust-mode
      :url "https://github.com/tree-sitter/tree-sitter-rust"
      :ext "\\.rs\\'")
    ,(make-use-package-treesit-recipe
      :lang 'scala
      :ts-mode 'scala-ts-mode
      :remap 'scala-mode
      :url "https://github.com/tree-sitter/tree-sitter-scala"
      :ext "\\.\\(scala\\|sbt\\)\\'")
    ,(make-use-package-treesit-recipe
      :lang 'sql
      :ts-mode 'sql-ts-mode
      :remap 'sql-mode
      :revision "gh-pages"
      :url "https://github.com/DerekStride/tree-sitter-sql"
      :ext "\\.sql\\'")
    ,(make-use-package-treesit-recipe
      :lang 'surface
      :ts-mode 'surface-ts-mode
      :remap 'surface-mode
      :url "https://github.com/connorlay/tree-sitter-surface")
    ,(make-use-package-treesit-recipe
      :lang 'toml
      :ts-mode 'toml-ts-mode
      :remap '(conf-toml-mode toml-mode)
      :url "https://github.com/tree-sitter/tree-sitter-toml"
      :ext "\\.toml\\'")
    ,(make-use-package-treesit-recipe
      :lang 'tsx
      :ts-mode 'tsx-ts-mode
      :remap '(typescript-tsx-mode)
      :requires 'typescript
      :url "https://github.com/tree-sitter/tree-sitter-typescript"
      :revision "master"
      :source-dir "tsx/src"
      :ext "\\.tsx\\'")
    ,(make-use-package-treesit-recipe
      :lang 'typescript
      :ts-mode 'typescript-ts-mode
      :remap 'typescript-mode
      :requires 'tsx
      :url "https://github.com/tree-sitter/tree-sitter-typescript"
      :revision "master"
      :source-dir "typescript/src"
      :ext "\\.ts\\'")
    ,(make-use-package-treesit-recipe
      :lang 'typst
      :ts-mode 'typst-ts-mode
      :remap 'typst-mode
      :url "https://github.com/uben0/tree-sitter-typst"
      :revision "master"
      :source-dir "src"
      :ext "\\.typ\\'")
    ,(make-use-package-treesit-recipe
      :lang 'verilog
      :ts-mode 'verilog-ts-mode
      :remap 'verilog-mode
      :url "https://github.com/gmlarumbe/tree-sitter-verilog"
      :ext "\\.s?vh?\\'")
    ,(make-use-package-treesit-recipe
      :lang 'vhdl
      :ts-mode 'vhdl-ts-mode
      :remap 'vhdl-mode
      :url "https://github.com/alemuller/tree-sitter-vhdl"
      :ext "\\.vhdl?\\'")
    ,(make-use-package-treesit-recipe
      :lang 'vue
      :ts-mode 'vue-ts-mode
      :remap 'vue-mode
      :url "https://github.com/tree-sitter-grammars/tree-sitter-vue"
      :ext "\\.vue\\'")
    ,(make-use-package-treesit-recipe
      :lang 'wast
      :ts-mode 'wat-ts-wast-mode
      :remap 'wat-mode
      :url "https://github.com/wasm-lsp/tree-sitter-wasm"
      :source-dir "wast/src"
      :ext "\\.wast\\'")
    ,(make-use-package-treesit-recipe
      :lang 'wat
      :ts-mode 'wat-ts-mode
      :remap 'wat-mode
      :url "https://github.com/wasm-lsp/tree-sitter-wasm"
      :source-dir "wat/src"
      :ext "\\.wat\\'")
    ,(make-use-package-treesit-recipe
      :lang 'wgsl
      :ts-mode 'wgsl-ts-mode
      :remap 'wgsl-mode
      :url "https://github.com/mehmetoguzderin/tree-sitter-wgsl"
      :ext "\\.wgsl\\'")
    ,(make-use-package-treesit-recipe
      :lang 'yaml
      :ts-mode 'yaml-ts-mode
      :remap 'yaml-mode
      :url "https://github.com/tree-sitter-grammars/tree-sitter-yaml"
      :ext "\\.ya?ml\\'"))
  "All the treesit languages that `use-package-treesit' can install automatically.")

(defvar use-package-treesit-keyword :treesit)

(defun use-package-normalize/:treesit (name-symbol _keyword _args)
  (list name-symbol))

(defun use-package-handler/:treesit (name-symbol _keyword _args rest state)
  (let* ((body (use-package-process-keywords name-symbol rest state))
         (new-body (use-package-concat
                    body
                    `((message "`use-package-treesit' was there")))))
    (message "Body after `use-package-treesit': %S" new-body)
    new-body))

(defun use-package-treesit/configure ()
  "Configure `use-package' for the `:treesit' keyword.

Insert the `:treesit' keyword (or whatever the value of the variable
`use-package-treesit-keyword' is) into the variable
`use-package-keywords' at the proper place, unless it is already there."
  (unless (member use-package-treesit-keyword use-package-keywords)
    (setq use-package-keywords
          (let* ((before-pos (cl-position :custom use-package-keywords))
                 (head (cl-subseq use-package-keywords 0 before-pos))
                 (tail (nthcdr before-pos use-package-keywords)))
            (append head `(,use-package-treesit-keyword) tail)))))

(use-package-treesit/configure)

(provide 'use-package-treesit)
;;; use-package-treesit.el ends here

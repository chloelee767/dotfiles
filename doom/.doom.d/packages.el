;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! magit-todos
  :pin "7294a95580bddf7232f2d205efae312dc24c5f61")

(package! groovy-mode)

(package! protobuf-mode
  :recipe (:local-repo "packages/protobuf"))

(package! nextflow-mode
  :recipe (:local-repo "packages/nextflow"))

;; (package! clang-format :pin "e48ff8ae18dc7ab6118c1f6752deb48cb1fc83ac")

(package! modus-themes :ignore (>= emacs-major-version 28))

(package! string-inflection
  :recipe (:host github :repo "akicho8/string-inflection" :files ("*.el"))
  :pin "617df25e91351feffe6aff4d9e4724733449d608")

(package! just-mode
  :recipe (:host github :repo "leon-barrett/just-mode.el" :files ("*.el"))
  :pin "310f437296173e4659c80490fcdb7ebafbac1665")

;; try instead: https://github.com/manzaltu/claude-code-ide.el

(package! claude-code-ide
  :recipe (:host github :repo "manzaltu/claude-code-ide.el")
  :pin "32d853e20b9d245a6ee89c4a153a4e568250c62c")

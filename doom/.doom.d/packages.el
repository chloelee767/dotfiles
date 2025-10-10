;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! magit-todos
  :pin "4c17b73355ad0f6537bec5776154ee7465a4c2f8")

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

;; (package! claude-code
;;   :recipe (:host github
;;            :repo "stevemolitor/claude-code.el"
;;            :branch "main"
;;            :files ("*.el" (:exclude "images/*")))
;;   :pin "158b38413040ae2b971ee722ebb0412ac174d6d6")

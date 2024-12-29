;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! org-download :pin "42ac361ef5502017e6fc1bceb00333eba90402f4")

(package! ox-gfm)

(package! groovy-mode)

(package! protobuf-mode
  :recipe (:local-repo "packages/protobuf"))

(package! nextflow-mode
  :recipe (:local-repo "packages/nextflow"))

;; (package! clang-format :pin "e48ff8ae18dc7ab6118c1f6752deb48cb1fc83ac")

(package! modus-themes :ignore (>= emacs-major-version 28))

(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :pin "c5dfa99f05878db5e6a6a378dc7ed09f11e803d4")

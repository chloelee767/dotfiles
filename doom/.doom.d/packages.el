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

(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :pin "c5dfa99f05878db5e6a6a378dc7ed09f11e803d4")

;; TODO setup
(package! copilot-chat
  :recipe (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :pin "8f0dcb8d21ff42b680bdc94aff89e9c3a6cbb8d8")

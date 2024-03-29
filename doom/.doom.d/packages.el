;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam")
  :pin "d95d25615e69e7cc847641800c1886366336c97e")
;; (package! org-roam :pin "78a371cdc4facf58fb20b513eaf60b24f3aaa7f7")
(package! org-fragtog :pin "92119e3ae7c9a0ae2b5c9d9e4801b5fdc4804ad7")
(package! org-superstar :pin "94f35c20f8b84a63defa145e3e6ae735fa33dd5d")
(package! org-download :pin "42ac361ef5502017e6fc1bceb00333eba90402f4")

(package! ox-gfm)

(package! groovy-mode)

(package! protobuf-mode
  :recipe (:local-repo "packages/protobuf"))

(package! nextflow-mode
  :recipe (:local-repo "packages/nextflow"))

(package! clang-format :pin "e48ff8ae18dc7ab6118c1f6752deb48cb1fc83ac")

(package! modus-themes :ignore (>= emacs-major-version 28))

(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el" "dist"))
  :pin "2c1c425761e3cdaf139c9e2fa531f942cfb14b16")

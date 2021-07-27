;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! org-roam
  :pin "de47f0a28d0e346c88c632ea1bb078b69d1e4b52")
;; (package! org-roam :pin "78a371cdc4facf58fb20b513eaf60b24f3aaa7f7")
(package! org-fragtog :pin "92119e3ae7c9a0ae2b5c9d9e4801b5fdc4804ad7")
(package! org-superstar :pin "94f35c20f8b84a63defa145e3e6ae735fa33dd5d")
(package! org-download :pin "42ac361ef5502017e6fc1bceb00333eba90402f4")
(package! org-transclusion
  :recipe (:host github
           :repo "nobiot/org-transclusion"
           :branch "main"
           :files ("*.el")))

(package! ox-gfm)

(package! groovy-mode)

(package! protobuf-mode
  :recipe (:local-repo "packages/protobuf"))

(package! nextflow-mode
  :recipe (:local-repo "packages/nextflow"))

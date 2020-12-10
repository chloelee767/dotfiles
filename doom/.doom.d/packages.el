;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; (package! org-gcal)
;; (package! org-fancy-priorities :disable t) ;; present for org +pretty

(package! org-fragtog :pin "92119e3ae7c9a0ae2b5c9d9e4801b5fdc4804ad7")

(package! org-ql :pin "e2dbeb462098bc70cae116a1a5ba78e20575c870")

(package! org-super-agenda :pin "857783ecd3dbe35c72b4eca046e0a5dc64041fdf")

(package! org-superstar :pin "94f35c20f8b84a63defa145e3e6ae735fa33dd5d")

;; biblio
(package! org-ref :pin "3f9d9fa096b97d81981bec6cc70b791b56e49f20")
(package! org-roam-bibtex :pin "a9a7d232ce25d06880aa2ed16148615af7e551a7")
(package! bibtex-completion :pin "8a0dd9841316793aacddea744d6b8ca4a7857a35")
(package! ivy-bibtex :pin "8a0dd9841316793aacddea744d6b8ca4a7857a35")

(package! org-download :pin "42ac361ef5502017e6fc1bceb00333eba90402f4")

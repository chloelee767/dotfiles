;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! org-ref)
(package! org-gcal)
(package! org-fancy-priorities :disable t) ;; present for org +pretty
;; (package! org-variable-pitch
;;   :recipe (:local-repo doom-private-dir
;;            :files ("org-variable-pitch.el")))

;; -*- no-byte-compile: t; -*-
;;; tools/gh-copilot/packages.el

(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :pin "6a2ad80489b8a0d021df95293eb7ac370aea140b")

;; (package! copilot-chat
;;   :recipe (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
;;   :pin "8f0dcb8d21ff42b680bdc94aff89e9c3a6cbb8d8")

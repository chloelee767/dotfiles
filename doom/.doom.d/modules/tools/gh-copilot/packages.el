;; -*- no-byte-compile: t; -*-
;;; tools/gh-copilot/packages.el

(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :pin "8a0068afcfb98af7f172d04f6f8cc932c1a22fe8")

;; (package! copilot-chat
;;   :recipe (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
;;   :pin "8f0dcb8d21ff42b680bdc94aff89e9c3a6cbb8d8")

;; -*- no-byte-compile: t; -*-
;;; lang/go/packages.el

(package! go-mode :pin "3273fcece5d9ab7edd4f15b2d6bce61f4e5a0666")
(package! gorepl-mode :pin "6a73bf352e8d893f89cad36c958c4db2b5e35e07")
(package! go-tag :pin "59b243f2fa079d9de9d56f6e2d94397e9560310a")
(package! go-gen-test :pin "35df36dcd555233ee1a618c0f6a58ce6db4154d9")

(when (and (modulep! :checkers syntax)
           (not (modulep! :checkers syntax +flymake)))
  (package! flycheck-golangci-lint :pin "8e446c68311048f0b87febf8ef0379e29d358851"))

;;; ../dotfiles/doom/.doom.d/packages/nextflow-mode.el -*- lexical-binding: t; -*-

(define-derived-mode nextflow-mode
  groovy-mode "Nextflow"
  "Major mode for editing Nextflow scripts and config files."
  (rainbow-delimiters-mode))

(add-to-list 'auto-mode-alist '("\\.nf\\'" . nextflow-mode))
(add-to-list 'auto-mode-alist '("nextflow\\.config\\'" . nextflow-mode))

;; insert nextflow.enable.dsl = 2 into new .nf files
(set-file-template! "\\.nf\\'"
  :trigger "dsl2"
  :mode 'nextflow-mode)

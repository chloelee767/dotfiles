;;; ../dotfiles/doom/.doom.d/nextflow-mode.el -*- lexical-binding: t; -*-

(define-derived-mode nextflow-mode
  groovy-mode "Nextflow"
  "Major mode for editing Nextflow scripts and config files."
  (rainbow-delimiters-mode))

(add-to-list 'auto-mode-alist '("\\.nf\\'" . nextflow-mode))
(add-to-list 'auto-mode-alist '("nextflow\\.config\\'" . nextflow-mode))

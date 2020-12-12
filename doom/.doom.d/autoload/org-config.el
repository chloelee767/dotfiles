;;; ../dotfiles/doom/.doom.d/autoload/org-config.el -*- lexical-binding: t; -*-

;;;###autoload
(defun chloe/toggle-org-roam-buffer-position ()
  (interactive)
  (setq org-roam-buffer-position (if (equal org-roam-buffer-position 'right) 'bottom 'right)))

;;; tools/org-roam/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +toggle-org-roam-buffer-position ()
  (interactive)
  (progn
  (setq org-roam-buffer-position (if (equal org-roam-buffer-position 'right) 'bottom 'right))
    (when (eq (org-roam-buffer--visibility) 'visible)
      (progn
        (org-roam-buffer-toggle-display)
        (org-roam-buffer-toggle-display)
        ))))

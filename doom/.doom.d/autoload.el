;;; ../dotfiles/doom/.doom.d/autoload.el -*- lexical-binding: t; -*-

;; ;;;###autoload
;; (defun chloe/toggle-org-roam-buffer-position ()
;;   (interactive)
;;   (progn
;;   (setq org-roam-buffer-position (if (equal org-roam-buffer-position 'right) 'bottom 'right))
;;     (when (eq (org-roam-buffer--visibility) 'visible)
;;       (progn
;;         (org-roam-buffer-toggle-display)
;;         (org-roam-buffer-toggle-display)
;;         ))))

;;;###autoload
(defun chloe/set-file-local-org-download-dir ()
  (interactive)
  (add-file-local-variable
   'org-download-image-dir
   (concat (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
           "_images")))

;;;###autoload
(defun chloe/yank-buffer-filename-only ()
  "Copy the current buffer's filename, exlucluding directories, to the kill ring."
  (interactive)
  (if-let (filename (or buffer-file-name (bound-and-true-p list-buffers-directory)))
      (message (kill-new (file-name-nondirectory filename)))
    (error "Couldn't find filename in current buffer")))

;;;###autoload
(defun chloe/chmod-current-file (new-file-modes)
  (interactive "sFile modes (octal or symbolic): ")
  (shell-command (concat "chmod " new-file-modes " " (buffer-file-name))))

;;;###autoload
(defun chloe/make-current-file-executable ()
  (interactive)
  (chloe/chmod-current-file "+x"))

;;;###autoload
(defun chloe/new-workspace-named (name)
  "Create a new workspace with the given NAME."
  (interactive "sWorkspace Name: ")
  (+workspace/new name))

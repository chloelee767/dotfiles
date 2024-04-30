;;; ../dotfiles/doom/.doom.d/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun chloe/set-file-local-org-download-dir ()
  "Still need to manually refresh the file local variables afterwards."
  (interactive)
  (if-let (filename (buffer-file-name))
      (progn
        (add-file-local-variable 'org-download-image-dir
                                 (concat (file-name-sans-extension (file-name-nondirectory filename)) "_images"))
        (message "Refresh file local variables now."))
    (error "Couldn't get filename of current buffer")))

;;;###autoload
(defun chloe/yank-buffer-filename-only ()
  "Copy the current buffer's filename, exlucluding directories, to the kill ring."
  (interactive)
  (if-let (filename (or buffer-file-name (bound-and-true-p list-buffers-directory)))
      (message (kill-new (file-name-nondirectory filename)))
    (error "Couldn't get filename of current buffer")))

;;;###autoload
(defun chloe/chmod-current-file ()
  "Change the file mode bits of the current file, similar to chmod. Interactively prompts for the file modes (eg. \"+x\")."
  (interactive)
  (chmod (buffer-file-name) (read-file-modes nil (buffer-file-name))))

;;;###autoload
(defun chloe/make-current-file-executable ()
  (interactive)
  (chmod (buffer-file-name) (file-modes-symbolic-to-number "+x" (file-modes (buffer-file-name)))))

;;;###autoload
(defun chloe/copy-last-kill-to-clipboard ()
  "Copy the last killed text to the system clipboard."
  (interactive)
  (with-temp-buffer
    (yank)
    (clipboard-kill-region (point-min) (point-max))))

;;;###autoload
(defun chloe/copy-clipboard-to-kill-ring ()
  "Copy clipboard contents to emacs kill ring."
  (interactive)
  (with-temp-buffer
    (clipboard-yank)
    (kill-region (point-min) (point-max))))

;;;###autoload
(defun chloe/vterm-yank-clipboard ()
  "Paste clipboard contents into vterm"
  (interactive)
  (progn (chloe/copy-clipboard-to-kill-ring) (vterm-yank)))

;;;###autoload
(defun chloe/project-search-symbol-at-point (&optional arg)
  (interactive "P")
  (+vertico/project-search arg (thing-at-point 'symbol)))


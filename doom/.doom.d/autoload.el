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
(defun chloe/org-roam-input-title-to-file-name (input)
  ;; ref.An Interesting Article => ref.an-interesting-article
  (downcase (string-replace
             "/" "-"
             (string-replace
              "^" ""
              (string-replace
               "\\." "."
               (string-replace
                " " "-"
                input))))))

;;;###autoload
(defun chloe/org-roam-input-title-to-file-title (input)
  ;; ref.An Interesting Article => An Interesting Article
  ;; ref => ref
  (if (string-match "\\." input)
      (file-name-extension input)
    input)) ;; case when top level

;;;###autoload (autoload 'org-roam-node-my-title "autoload" nil t)
(cl-defmethod org-roam-node-my-title ((node org-roam-node))
  (file-name-sans-extension (file-relative-name (org-roam-node-file node) org-roam-directory)))

;;;###autoload
(defun chloe/org-roam-node-find-children (&optional other-window)
  ;; org-roam-node-find with intial input set so that only children of the current file are listed
  (interactive current-prefix-arg)
  (if (org-roam-file-p buffer-file-name)
      (let ((note-name (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
        (org-roam-node-find other-window (concat "^" note-name "\\.")))
    (error "Not in an org roam file")))

;;;###autoload
(defun chloe/org-roam-node-find-siblings ()
  (interactive)
  (if (org-roam-file-p buffer-file-name)
      (let* ((notename (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
             (parent-notename (chloe/org-roam-note-parent notename)))
        (if parent-notename
            (org-roam-node-find nil (concat "^" parent-notename "\\."))
          (message "Root level note, no parent")))
    (error "Not in an org roam file")))

;;;###autoload
(defun chloe/org-roam-node-find-parent ()
  (interactive)
  (if (org-roam-file-p buffer-file-name)
      (let* ((notename (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
             (parent-notename (chloe/org-roam-note-parent notename)))
        (if parent-notename
            (org-roam-node-find nil (concat "^" parent-notename))
          (message "Root level note, no parent")))
    (error "Not in an org roam file")))

;; TODO replace all . with \\.
;; TODO automatically visit if there is exactly 1 match in org-roam-node-find
;; TODO more intelligent collision detection -- overwrite empty files

;;; ../dotfiles/doom/.doom.d/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun chloe/org-roam-file-slug (input)
  "My version of `org-roam-node-slug'
- Replace spaces with -
- Downcase
- Ensures no trailing or sequential underscores"
  ;; Adapted from `org-roam-node-slug'
  (let ((slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                           768 ; U+0300 COMBINING GRAVE ACCENT
                           769 ; U+0301 COMBINING ACUTE ACCENT
                           770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                           771 ; U+0303 COMBINING TILDE
                           772 ; U+0304 COMBINING MACRON
                           774 ; U+0306 COMBINING BREVE
                           775 ; U+0307 COMBINING DOT ABOVE
                           776 ; U+0308 COMBINING DIAERESIS
                           777 ; U+0309 COMBINING HOOK ABOVE
                           778 ; U+030A COMBINING RING ABOVE
                           780 ; U+030C COMBINING CARON
                           795 ; U+031B COMBINING HORN
                           803 ; U+0323 COMBINING DOT BELOW
                           804 ; U+0324 COMBINING DIAERESIS BELOW
                           805 ; U+0325 COMBINING RING BELOW
                           807 ; U+0327 COMBINING CEDILLA
                           813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                           814 ; U+032E COMBINING BREVE BELOW
                           816 ; U+0330 COMBINING TILDE BELOW
                           817 ; U+0331 COMBINING MACRON BELOW
                           )))
    (cl-flet* ((nonspacing-mark-p (char)
                                  (memq char slug-trim-chars))
               (strip-nonspacing-marks (s)
                                       (ucs-normalize-NFC-string
                                        (apply #'string (seq-remove #'nonspacing-mark-p
                                                                    (ucs-normalize-NFD-string s)))))
               (cl-replace (title pair)
                           (replace-regexp-in-string (car pair) (cdr pair) title)))
      (let* ((pairs `(("[^[:alnum:][:digit:].]" . "-")  ;; convert anything not alphanumeric or .
                      ("--*" . "-")  ;; remove sequential dashes
                      ("-\\.*" . ".")
                      ("\\.-*" . ".")
                      ("^-" . "")  ;; remove starting dash
                      ("-$" . "") ;; remove ending dash
                      ("^\\." . "")
                      ("\\.$" . "")))
             (slug (-reduce-from #'cl-replace (strip-nonspacing-marks input) pairs)))
        (downcase slug)))))

;;;###autoload
(defun chloe/org-roam-title (slug)
  (file-name-extension slug))

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

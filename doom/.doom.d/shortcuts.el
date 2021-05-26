;;; ../dotfiles/doom/.doom.d/shortcuts.el -*- lexical-binding: t; -*-

(map! :leader :prefix "f"
      :desc "Yank filename (no directories)" "Y" #'chloe/yank-buffer-filename-only
      :desc "chmod" "x" #'chloe/chmod-current-file
      :desc "make executable" "X" #'chloe/make-current-file-executable)

;; open things
(map! "<f1>" #'org-agenda
      "<f2>" #'(lambda () (interactive)(doom-project-browse chloe/org-agenda-directory)))
(map! :leader (:prefix "o"
               :desc "Google calendar" "c" #'(lambda () (interactive)(browse-url "https://calendar.google.com"))))

;; shortcuts to useful folders
;; SPC f f is already bound counsel-find-file, but I don't mind since find file is bound to SPC . as well
(map! :leader (:prefix "f" "f" nil))
(map! :leader (:prefix "f" (:prefix ("f" . "favourites")
                            :desc "Home" "h" (lambda () (interactive)(doom-project-browse "~/"))
                            :desc "Documents" "d" (lambda () (interactive)(doom-project-browse chloe/documents-directory))
                            :desc "Dropbox" "r" (lambda () (interactive (doom-project-browse "~/Dropbox/")))
                            :desc "Code" "c" (lambda () (interactive)(doom-project-browse "~/Code/"))
                            :desc "Dotfiles" "t" (lambda () (interactive)(doom-project-browse "~/dotfiles/"))
                            :desc "Dotfiles (find in project)" "T" (lambda () (interactive)(doom-project-find-file "~/dotfiles/"))
                            :desc "Agenda folder" "a" (lambda () (interactive)(doom-project-browse chloe/org-agenda-directory))
                            :desc "Org folder" "o" (lambda () (interactive)(doom-project-browse org-directory))
                            :desc "NUS" "n" (lambda () (interactive)(doom-project-browse chloe/nus-directory))
                            :desc "Current semester" "s" (lambda () (interactive)(doom-project-browse chloe/nus-current-sem-directory))
                            :desc "UROPS" "u" (lambda () (interactive)(doom-project-browse chloe/urops-directory))
                            :desc "NSCC server" "e" (lambda () (interactive)(doom-project-browse "/ssh:e0325190@nus.nscc.sg:/home/users/nus/e0325190/"))
                            :desc "Work" "w" (lambda () (interactive)(doom-project-browse "~/Dropbox/Work/"))
                            )))

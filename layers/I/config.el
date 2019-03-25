(defvar org-agenda-dir ""
  "gtd org files location")

(defvar deft-dir ""
  "deft org files locaiton")

(defvar blog-admin-dir ""
  "blog-admin files location")

(if (spacemacs/system-is-mswindows)
    (setq
     org-agenda-dir "f:/org-notes"
     deft-dir "f:/org-notes"
     )
  (setq
   org-agenda-dir "~/Dropbox/Emacs/GTD"
   deft-dir "~/Dropbox/Emacs/Notes/"
   ))

(setq org-journal-dir "~/Dropbox/Emacs/Journal")
;; for ranger options
(setq ranger-parent-depth 1)
(setq ranger-ignored-extensions '("mkv" "iso" "mp4"))
(setq ranger-show-hidden nil)
(setq ranger-modify-header t)
(setq ranger-max-preview-size 6)
(setq ranger-dont-show-binary nil)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
;; title format, "filename  @  whole path"
(setq frame-title-format '("%b   @   " buffer-file-name))
(display-time)
(setq display-time-day-and-date t)

(add-hook 'python-mode-hook 'highlight-indentation-mode)
(add-hook 'java-mode-hook 'highlight-indentation-mode)
(add-hook 'go-mode-hook 'highlight-indentation-mode)
(add-hook 'scala-mode-hook 'highlight-indentation-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-indentation-mode)
(add-hook 'sh-mode-hook 'highlight-indentation-mode)

(add-hook 'python-mode-hook 'tooltip-mode)
(add-hook 'go-mode-hook 'tooltip-mode)

;;load sensitive data
;; or (like spacemacs init.el)put the above variable into it ,then the own value separated from public config
;; .emacs.secrets.el for example:
;; (setq-default
;;  org-agenda-dir "~/Dropbox/Apps/emacs/gtd"
;;  deft-dir "~/Dropbox/Apps/emacs/notes"
;;  blog-admin-dir "~/Documents/hexo"
;;  )
;; (slack-register-team
;;   :name "emacs-slack"
;;   :default t
;;   :client-id "xxxxxxxxx"
;;   :client-secret "xxxxxxxxx"
;;   :token "xxxxxxxxx"
;;   :subscribed-channels '(xxxxxxxxx))
;; (setq paradox-github-token "")
;; (load "~/Dropbox/Apps/emacs/.emacs.secrets.el" t)

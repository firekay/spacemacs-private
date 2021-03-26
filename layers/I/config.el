;; ##################################
;; For debug
;; (setq google-translate-backend-debug t)
;; (setq google-translate-backend-user-agent "Emacs")
;; ##################################


(fset 'I/delete-empty-lines (kbd "M-x flush-lines RET ^\s-*$ RET"))
;; ########################## for sql connnect
;; ########################## for sql connnect
;; 1. add a connection infomation in sql-connection-alist
;; 2. define a function call I/sql-connnect. e.g.: I/pg_local
;; 3. add a config in I/sql-servers-list
(setq sql-connection-alist
      '((mysql-14 (sql-product 'mysql)
                  (sql-port 3305)
                  (sql-server "127.0.0.1")
                  (sql-user "admin")
                  (sql-password "shopee123")
                  (sql-database ""))
        (mysql-nb (sql-product 'mysql)
                  (sql-port 3307)
                  (sql-server "127.0.0.1")
                  (sql-user "root")
                  (sql-password "root")
                  (sql-database ""))
        ))

(setq I/sql-servers-list
  '(("mysql-14" I/mysql-14)
    ("mysql-nb" I/mysql-nb))
  )

(defun I/mysql-14 ()
     (interactive)
     (I/sql-connect 'postgres 'mysql-14))

(defun I/mysql-nb ()
  (interactive)
  (I/sql-connect 'postgres 'mysql-nb))

;; do not need modify
(defun I/sql-connect-server (func)
     "Connect to the input server using my-sql-servers-list"
     (interactive
          (helm-comp-read "Select server: " I/sql-servers-list))
     (funcall func))

(defun I/sql-connect (product connection)
     ;; remember to set the sql-product, otherwise, it will fail for the first time
     ;; you call the function
     (setq sql-product product)
     (if current-prefix-arg
                (sql-connect connection connection)
              (sql-connect connection))
     (sql-connect connection))


;; move to dropbox
;; ########################## sql connnect end


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
   org-agenda-dir "~/Dropbox/Beorg/GTD"
   deft-directory "~/Dropbox/Emacs/Notes/"
   ))

;; for google translate
(setq google-translate-backend-method 'curl)

(setq org-use-sub-superscripts (quote {}))
(setq org-export-with-sub-superscripts (quote {}))
(setq org-journal-dir "~/Dropbox/Emacs/Journal")
(setq blacken-line-length 99)
(setq markdown-command "pandoc")
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

;; (add-hook 'python-mode-hook 'highlight-indentation-mode)
;; (add-hook 'java-mode-hook 'highlight-indentation-mode)
;; (add-hook 'go-mode-hook 'highlight-indentation-mode)
;; (add-hook 'scala-mode-hook 'highlight-indentation-mode)
;; (add-hook 'emacs-lisp-mode-hook 'highlight-indentation-mode)
;; (add-hook 'sh-mode-hook 'highlight-indentation-mode)

(add-hook 'python-mode-hook 'tooltip-mode)
(add-hook 'go-mode-hook 'tooltip-mode)

(add-hook 'rust-mode-hook #'flycheck-mode)
(add-hook 'go-mode-hook #'flycheck-mode)
(add-hook 'shell-mode-hook #'flycheck-mode)

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

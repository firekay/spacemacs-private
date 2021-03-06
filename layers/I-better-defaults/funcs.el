;;; funcs.el --- I Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 I 
;;
;; URL: https://github.com/firekay/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; for login remote server using vterm(using list servers)
(defun I/ssh-server (host)
  (interactive
   (helm-comp-read "Select server: " I/ssh-remote-servers))
  (I/ssh-vterm host))

;; for login remote server using vterm
(defun I/ssh-vterm (host)
  (interactive "sHost: (default kg-dev)")
  (let* ((host (if (equal host "") "kg-dev" host))
         (default-directory (concat"/ssh:zhenkai.xu@" host ":~")))
    (vterm-toggle-cd)))

;; for login remote server
(defun I/ssh-remote (host)
  "Connect to a remote host by SSH."
  (interactive "sHost: (default kg-dev)")
  (let* ((host (if (equal host "") "kg-dev" host))
         (switches (list host "-l" "zhenkai.xu" "-p" "22")))
    (set-buffer (apply 'make-term host "ssh" nil switches))
    (term-mode)
    (term-char-mode)
    (switch-to-buffer (concat "*" host "*"))))


;; ;; for login remote server
;; (defun I/remote-ssh (user host port)
;;   "Connect to a remote host by SSH."
;;   (interactive "sUser: (default zhenkai.xu) \nsHost: \nsPort (default 22): ")
;;   (let* ((port (if (equal port "") "22" port))
;;          (user (if (equal user "") "zhenkai.xu" user))
;;          (switches (list host "-l" user "-p" port)))
;;     (set-buffer (apply 'make-term host "ssh" nil switches))
;;     (term-mode)
;;     (term-char-mode)
;;     (switch-to-buffer (concat "*" host "*"))))

;; for open emacs vterm
(defun I/vterm-graphx()
  (interactive)
  (find-file "~/src/shopee/knowledge-graph/kg-graphx/.graphx.term")
  )

(I/vterm-graphx)

(defun I/directory-parent (directory)
  (let ((parent (file-name-directory (directory-file-name directory))))
    (if (not (equal directory parent))
        parent)))

(defun I/chomp (str)
  "Trim leading and trailing whitespace from STR."
  (replace-regexp-in-string "\\(\\`[[:space:]\n]*\\|[[:space:]\n]*\\'\\)" "" str))

(defun I/find-file-remote (host &optional path)
  (interactive "sHost: \nsPath(default ~/): ")
  (find-file (concat "/ssh:zhenkai.xu@" (I/chomp host) ":" path))
  )

(defun I/find-file-remote-root (host &optional path)
  (interactive "sHost: \nsPath(default /root): ")
  (find-file (concat "/ssh:zhenkai.xu@" (I/chomp host) "|sudo:" host ":" path))
  )

(defun I/now-today (arg)
  (interactive "P")
  (insert (if arg
              (format-time-string "%d-%m-%Y %a")
            (format-time-string "%Y-%m-%d %a"))))

(defun I/now (arg)
  (interactive "P")
  (insert (if arg
              (format-time-string "%d-%m-%Y %H:%M %a")
            (format-time-string "%Y-%m-%d %H:%M %a"))))


(defun I/now-timestamp (arg)
  (interactive "P")
  (insert (if arg
              (format-time-string "%d-%m-%Y %H:%M:%d %a")
            (format-time-string "%Y-%m-%d %H:%M:%d %a"))))


(defun I/now-time (arg)
  (interactive "P")
  (insert (if arg
              (format-time-string "%H:%M:%d")
            (format-time-string "%H:%M:%d"))))

(defun I/now-hour-min (arg)
  (interactive "P")
  (insert (if arg
              (format-time-string "%H:%M")
            (format-time-string "%H:%M"))))

;; for hs-hide-leaves node
(defun I/hs-hide-leaves-recursive (minp maxp)
  "Hide blocks below point that do not contain further blocks in
    region (MINP MAXP)."
  (when (hs-find-block-beginning)
    (setq minp (1+ (point)))
    (funcall hs-forward-sexp-func 1)
    (setq maxp (1- (point))))
  (unless hs-allow-nesting
    (hs-discard-overlays minp maxp))
  (goto-char minp)
  (let ((leaf t))
    (while (progn
             (forward-comment (buffer-size))
             (and (< (point) maxp)
                  (re-search-forward hs-block-start-regexp maxp t)))
      (setq pos (match-beginning hs-block-start-mdata-select))
      (if (I/hs-hide-leaves-recursive minp maxp)
          (save-excursion
            (goto-char pos)
            (hs-hide-block-at-point t)))
      (setq leaf nil))
    (goto-char maxp)
    leaf))

;; for fold leaf nodes
(defun I/hs-hide-leaves ()
  "Hide all blocks in the buffer that do not contain subordinate
    blocks.  The hook `hs-hide-hook' is run; see `run-hooks'."
  (interactive)
  (hs-life-goes-on
   (save-excursion
     (message "Hiding blocks ...")
     (save-excursion
       (goto-char (point-min))
       (I/hs-hide-leaves-recursive (point-min) (point-max)))
     (message "Hiding blocks ... done"))
   (run-hooks 'hs-hide-hook)))

(defun I/indent-buffer()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun I/toggle-indent-guide-recursive()
  (interactive)
  (if (not indent-guide-recursive)
      (setq indent-guide-recursive t)
    (setq indent-guide-recursive nil)))

(defun I/indent-region-or-buffer()
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (I/indent-region (region-beginning) (region-end))
          (message "Indent selected region."))
      (progn
        (I/indent-buffer)
        (message "Indent buffer.")))))

;;http://emacsredux.com/blog/2013/03/26/smarter-open-line/
(defun I/smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))


(defun I/rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun I/yank-to-end-of-line ()
  "Yank to end of line."
  (interactive)
  (evil-yank (point) (point-at-eol)))

(defun I/occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (deactivate-mark)
  (call-interactively 'occur))

(defun I/occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

(defun I/dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message
       "Size of all marked files: %s"
       (progn
         (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
         (match-string 1))))))

(defun I/dired-start-process (cmd &optional file-list)
  (interactive
   (let ((files (dired-get-marked-files
                 t current-prefix-arg)))
     (list
      (dired-read-shell-command "& on %s: "
                                current-prefix-arg files)
      files)))
  (let (list-switch)
    (start-process
     cmd nil shell-file-name
     shell-command-switch
     (format
      "nohup 1>/dev/null 2>/dev/null %s \"%s\""
      (if (and (> (length file-list) 1)
               (setq list-switch
                     (cadr (assoc cmd dired-filelist-cmd))))
          (format "%s %s" cmd list-switch)
        cmd)
      (mapconcat #'expand-file-name file-list "\" \"")))))

(defun I/dired-open-term ()
  "Open an `ansi-term' that corresponds to current directory."
  (interactive)
  (let* ((current-dir (dired-current-directory))
         (buffer (if (get-buffer "*zshell*")
                     (switch-to-buffer "*zshell*")
                   (ansi-term "/bin/zsh" "zshell")))
         (proc (get-buffer-process buffer)))
    (term-send-string
     proc
     (if (file-remote-p current-dir)
         (let ((v (tramp-dissect-file-name current-dir t)))
           (format "ssh %s@%s\n"
                   (aref v 1) (aref v 2)))
       (format "cd '%s'\n" current-dir)))))

(defun I/dired-copy-file-here (file)
  (interactive "fCopy file: ")
  (copy-file file default-directory))

(defun I/dired-find-file ()
  "Open buffer in another window"
  (interactive)
  (let ((filename (dired-get-filename nil t)))
    (if (car (file-attributes filename))
        (dired-find-alternate-file)
      (dired-find-file-other-window))))

(defun I/dired-do-command (command)
  "Run COMMAND on marked files. Any files not already open will be opened.
After this command has been run, any buffers it's modified will remain
open and unsaved."
  (interactive "CRun on marked files M-x ")
  (save-window-excursion
    (mapc (lambda (filename)
            (find-file filename)
            (call-interactively command))
          (dired-get-marked-files))))

(defun I/dired-up-directory()
  "goto up directory and resue buffer"
  (interactive)
  (find-alternate-file ".."))

(defun I/insert-space-after-point ()
  (interactive)
  (save-excursion (insert " ")))


(defmacro I-define-up/downcase-dwim (case)
  (let ((func (intern (concat "I/" case "-dwim")))
        (doc (format "Like `%s-dwim' but %s from beginning when no region is active." case case))
        (case-region (intern (concat case "-region")))
        (case-word (intern (concat case "-word"))))
    `(defun ,func (arg)
       ,doc
       (interactive "*p")
       (save-excursion
         (if (use-region-p)
             (,case-region (region-beginning) (region-end))
           (beginning-of-thing 'symbol)
           (,case-word arg))))))

(I-define-up/downcase-dwim "upcase")
(I-define-up/downcase-dwim "downcase")
(I-define-up/downcase-dwim "capitalize")

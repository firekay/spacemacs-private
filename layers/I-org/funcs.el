;;; funcs.el --- I Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 I
;;
;; URL: https://github.com/I/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(require 'cl)


;; es-mode with socks
(defun org-babel-expand-body:es (body params)
  "This command is used by org-tangle to create a file with the
source code of the elasticsearch block. If :tangle specifies a
file with the .sh extension a curl-request is created instead of
just a normal .es file that contains the body of the block.."
  (let ((ext (file-name-extension
              (cdr (assoc :tangle params))))
        (body (s-format body 'es-org-aget
                        (mapcar (lambda (x)
                                  (when (eq (car x) :var)
                                    (cdr x)))
                                params))))
    (if (not (equal "sh" ext))
        body
      (let ((method (cdr (assoc :method params)))
            (url (cdr (assoc :url params))))
        (format "curl --socks5 localhost:9876 -X%s %s -d %S\n"
                (upcase method)
                url
                body)))))

;; es-mode without socks
(defun org-babel-expand-body:es (body params)
  "This command is used by org-tangle to create a file with the
source code of the elasticsearch block. If :tangle specifies a
file with the .sh extension a curl-request is created instead of
just a normal .es file that contains the body of the block.."
  (let ((ext (file-name-extension
              (cdr (assoc :tangle params))))
        (body (s-format body 'es-org-aget
                        (mapcar (lambda (x)
                                  (when (eq (car x) :var)
                                    (cdr x)))
                                params))))
    (if (not (equal "sh" ext))
        body
      (let ((method (cdr (assoc :method params)))
            (url (cdr (assoc :url params))))
        (format "curl -X%s %s -d %S\n"
                (upcase method)
                url
                body)))))



;; Screenshot
(defun I/insert-org-or-md-img-link (prefix imagename)
  (if (equal (file-name-extension (buffer-file-name)) "org")
      (insert (format "[[%s%s]]" prefix imagename))
    (insert (format "![%s](%s%s)" imagename prefix imagename))))

(defun I/capture-screenshot (basename)
  "Take a screenshot into a time stamped unique-named file in the
  same directory as the org-buffer/markdown-buffer and insert a link to this file."
  (interactive "sScreenshot name: ")
  (if (equal basename "")
      (setq basename (format-time-string "%Y%m%d_%H%M%S")))
  (setq filename (file-name-base (buffer-file-name)))
  (setq path-with-name (concat filename "." basename ".png"))
  (setq img-directory "img")
  (unless (file-directory-p "./img")
    (make-directory img-directory t))
  (call-process "screencapture"
                nil
                nil
                nil
                "-s"
                (concat "img/" path-with-name))
  (I/insert-org-or-md-img-link "./img/"
                                (concat path-with-name))
  (insert "\n"))

(defun I/org-archive-tasks ()
  (interactive)
  (I/org-archive-done-tasks)
  (I/org-archive-cancel-tasks))

(defun I/org-archive-done-tasks ()
  (interactive)
  (org-map-entries (lambda ()
                     (org-archive-subtree)
                     (setq org-map-continue-from (outline-previous-heading)))
                   "/DONE"
                   'file))

(defun I/org-archive-cancel-tasks ()
  (interactive)
  (org-map-entries (lambda ()
                     (org-archive-subtree)
                     (setq org-map-continue-from (outline-previous-heading)))
                   "/CANCELLED"
                   'file))

;; "https://github.com/vhallac/.emacs.d/blob/master/config/customize-org-agenda.el"
(defun I/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (bh/list-sublevels-for-projects-indented)
  (save-restriction (widen)
                    (let ((next-headline (save-excursion
                                           (or (outline-next-heading)
                                               (point-max)))))
                      ;; VH: I changed this line from
                      ;; (if (bh/is-project-p)
                      (if (and (eq (point) (bh/find-project-task))
                               (bh/is-project-p))
                          (let* ((subtree-end (save-excursion
                                                (org-end-of-subtree t)))
                                 (has-next))
                            (save-excursion
                              (forward-line 1)
                              (while (and (not has-next)
                                          (< (point) subtree-end)
                                          (re-search-forward "^\\*+ NEXT " subtree-end
                                                             t))
                                (unless (member "WAITING" (org-get-tags-at))
                                  (setq has-next t))))
                            (if has-next next-headline nil)) ; a stuck project, has subtasks but no next task
                        next-headline))))

(defun I/org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive (let ((src-code-types '("python" "go" "rust" "sh" "scala" "java"
                                       "js" "emacs-lisp" "clojure" "C++" "css" "calc"
                                       "asymptote" "dot" "gnuplot" "ledger" "lilypond"
                                       "mscgen" "octave" "oz" "plantuml" "R" "sass"
                                       "screen" "sql" "awk" "ditaa" "haskell" "latex"
                                       "lisp" "matlab" "ocaml" "org" "perl" "ruby"
                                       "scheme" "sqlite" "C")))
                 (list (ido-completing-read "Source code type: "
                                            src-code-types))))
  (progn
    (evil-end-of-line)
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

(defun org-reset-subtask-state-subtree ()
  "Reset all subtasks in an entry subtree."
  (interactive "*")
  (if (org-before-first-heading-p)
      (error "Not inside a tree")
    (save-excursion
      (save-restriction (org-narrow-to-subtree)
                        (org-show-subtree)
                        (goto-char (point-min))
                        (beginning-of-line 2)
                        (narrow-to-region (point)
                                          (point-max))
                        (org-map-entries '(when (member (org-get-todo-state) org-done-keywords)
                                            (org-todo (car org-todo-keywords))))))))

(defun org-reset-subtask-state-maybe ()
  "Reset all subtasks in an entry if the `RESET_SUBTASKS' property is set"
  (interactive "*")
  (if (org-entry-get (point)
                     "RESET_SUBTASKS")
      (org-reset-subtask-state-subtree)))

(defun org-subtask-reset ()
  (when (member org-state org-done-keywords) ;; org-state dynamically bound in org.el/org-todo
    (org-reset-subtask-state-maybe)
    (org-update-statistics-cookies t)))

(defun I/org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states) ; turn off logging
    (org-todo (if (= n-not-done 0)
                  "DONE"
                "TODO"))))

(defun I/filter-by-tags ()
  (let ((head-tags (org-get-tags-at)))
    (member current-tag head-tags)))

(defun I/org-clock-sum-today-by-tags (timerange &optional tstart tend noinsert)
  (interactive "P")
  (let* ((timerange-numeric-value (prefix-numeric-value timerange))
         (files (org-add-archive-files (org-agenda-files)))
         (include-tags '("WORK" "EMACS" "DREAM" "WRITING" "MEETING"
                         "LIFE" "PROJECT" "OTHER"))
         (tags-time-alist (mapcar (lambda (tag)
                                    `(,tag . 0))
                                  include-tags))
         (output-string "")
         (tstart (or tstart
                     (and timerange
                          (equal timerange-numeric-value 4)
                          (- (org-time-today)
                             86400))
                     (and timerange
                          (equal timerange-numeric-value 16)
                          (org-read-date nil nil nil "Start Date/Time:"))
                     (org-time-today)))
         (tend (or tend
                   (and timerange
                        (equal timerange-numeric-value 16)
                        (org-read-date nil nil nil "End Date/Time:"))
                   (+ tstart 86400)))
         h
         m
         file
         item
         prompt
         donesomething)
    (while (setq file (pop files))
      (setq org-agenda-buffer (if (file-exists-p file)
                                  (org-get-agenda-file-buffer file)
                                (error "No such file %s" file)))
      (with-current-buffer org-agenda-buffer
        (dolist (current-tag include-tags)
          (org-clock-sum tstart tend 'I/filter-by-tags)
          (setcdr (assoc current-tag tags-time-alist)
                  (+ org-clock-file-total-minutes
                     (cdr (assoc current-tag tags-time-alist)))))))
    (while (setq item (pop tags-time-alist))
      (unless (equal (cdr item) 0)
        (setq donesomething t)
        (setq h (/ (cdr item)
                   60)
              m
              (- (cdr item)
                 (* 60 h)))
        (setq output-string (concat output-string
                                    (format "[-%s-] %.2d:%.2d\n"
                                            (car item)
                                            h
                                            m)))))
    (unless donesomething
      (setq output-string (concat output-string "[-Nothing-] Done nothing!!!\n")))
    (unless noinsert
      (insert output-string))
    output-string))

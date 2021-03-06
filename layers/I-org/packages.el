;;
;; Copyright (c) 2014-2016 I
;;
;; URL: https://github.com/I/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst I-org-packages '((org :location built-in)
                           ;; org-mac-link deft
                           ;; (blog-admin :location (recipe
                           ;;                        :fetcher github
                           ;;                        :repo "codefalling/blog-admin"))
                           ;; org-tree-slide
                           ;; ox-reveal
                           ;; worf
                           ;; org-download
                           ;; plain-org-wiki
                           ))

(defun I-org/init-blog-admin ()
  (use-package blog-admin
    :defer t
    :commands blog-admin-start
    :init (progn
            ;; do your configuration here
            (setq blog-admin-backend-type 'hexo blog-admin-backend-path
                  blog-admin-dir blog-admin-backend-new-post-with-same-name-dir
                  nil blog-admin-backend-hexo-config-file "_config.yml")
            (add-hook 'blog-admin-backend-after-new-post-hook
                      'find-file))))

(defun I-org/post-init-org-pomodoro ()
  (I/pomodoro-notification))

;;In order to export pdf to support Chinese, I should install Latex at here: https://www.tug.org/mactex/
;; http://freizl.github.io/posts/2012-04-06-export-orgmode-file-in-Chinese.html
;;http://stackoverflow.com/questions/21005885/export-org-mode-code-block-and-result-with-different-styles
(defun I-org/post-init-org ()
  (add-hook 'org-mode-hook
            (lambda ()
              (spacemacs/toggle-line-numbers-off))
            'append)
  (with-eval-after-load 'org
    (progn
      (spacemacs|disable-company org-mode)
      (require 'org-compat)
      (require 'org)
      ;; (add-to-list 'org-modules "org-habit")
      (add-to-list 'org-modules 'org-habit)
      (require 'org-habit)
      (setq org-refile-use-outline-path 'file)
      (setq org-outline-path-complete-in-steps nil)
      (setq org-refile-targets '((nil :maxlevel .
                                      4)
                                 (org-agenda-files :maxlevel .
                                                   4)))
      ;; config stuck project
      (setq org-stuck-projects '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))
      (setq org-agenda-inhibit-startup t) ;; ~50x speedup
      (setq org-agenda-span 'day)
      (setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
      (setq org-agenda-window-setup 'current-window)
      (setq org-log-done t)
      ;; 加密文章
      ;; "http://coldnew.github.io/blog/2013/07/13_5b094.html"
      ;; org-mode 設定
      (require 'org-crypt)
      ;; need install gpg-suite(gnupg?)
      ;; 當被加密的部份要存入硬碟時，自動加密回去
      (org-crypt-use-before-save-magic)
      ;; 設定要加密的 tag 標籤為 secret
      (setq org-crypt-tag-matcher "CRYPT")
      ;; 避免 secret 這個 tag 被子項目繼承 造成重複加密
      ;; (但是子項目還是會被加密喔)
      (setq org-tags-exclude-from-inheritance (quote ("CRYPT")))
      ;; 用於加密的 GPG 金鑰
      ;; 可以設定任何 ID 或是設成 nil 來使用對稱式加密 (symmetric encryption)
      (setq org-crypt-key nil)
      ;; (add-to-list 'auto-mode-alist '("\.org\\'" . org-mode))
      (setq org-todo-keywords (quote ((sequence "TODO(t)" "STARTED(s)" "|"
                                                "CANCELLED(c@/!)" "DONE(d!/!)")
                                      (sequence "WAITING(w@/!)" "SOMEDAY(S)" "|"
                                                "MEETING(m)" "PHONE(p)"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Org clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; Change task state to STARTED when clocking in
      (setq org-clock-in-switch-to-state "STARTED")
      ;; Save clock data and notes in the LOGBOOK drawer
      (setq org-clock-into-drawer t)
      ;; Removes clocked tasks with 0:00 duration
      (setq org-clock-out-remove-zero-time-clocks
            t) ;; Show the clocked-in task - if any - in the header line
      (setq org-tags-match-list-sublevels nil)
      (add-hook 'org-mode-hook
                '(lambda ()
                   ;; keybinding for editing source code blocks
                   ;; keybinding for inserting code blocks
                   (local-set-key (kbd "C-c i s")
                                  'I/org-insert-src-block)))
      (require 'ox-publish)
      (add-to-list 'org-latex-classes
                   '("ctexart" "\\documentclass[11pt]{ctexart}
                                        [NO-DEFAULT-PACKAGES]
                                        \\usepackage[utf8]{inputenc}
                                        \\usepackage[T1]{fontenc}
                                        \\usepackage{fixltx2e}
                                        \\usepackage{graphicx}
                                        \\usepackage{longtable}
                                        \\usepackage{float}
                                        \\usepackage{wrapfig}
                                        \\usepackage{rotating}
                                        \\usepackage[normalem]{ulem}
                                        \\usepackage{amsmath}
                                        \\usepackage{textcomp}
                                        \\usepackage{marvosym}
                                        \\usepackage{wasysym}
                                        \\usepackage{amssymb}
                                        \\usepackage{booktabs}
                                        \\usepackage[colorlinks,linkcolor=black,anchorcolor=black,citecolor=black]{hyperref}
                                        \\tolerance=1000
                                        \\usepackage{listings}
                                        \\usepackage{xcolor}
                                        \\lstset{
                                        %行号
                                        numbers=left,
                                        %背景框
                                        framexleftmargin=10mm,
                                        frame=none,
                                        %背景色
                                        %backgroundcolor=\\color[rgb]{1,1,0.76},
                                        backgroundcolor=\\color[RGB]{245,245,244},
                                        %样式
                                        keywordstyle=\\bf\\color{blue},
                                        identifierstyle=\\bf,
                                        numberstyle=\\color[RGB]{0,192,192},
                                        commentstyle=\\it\\color[RGB]{0,96,96},
                                        stringstyle=\\rmfamily\\slshape\\color[RGB]{128,0,0},
                                        %显示空格
                                        showstringspaces=false
                                        }
                                        "
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
      ;; {{ export org-mode in Chinese into PDF
      ;; @see http://freizl.github.io/posts/tech/2012-04-06-export-orgmode-file-in-Chinese.html
      ;; and you need install texlive-xetex on different platforms
      ;; To install texlive-xetex:
      ;;    `sudo USE="cjk" emerge texlive-xetex` on Gentoo Linux
      ;; }}

      (setq org-latex-default-class "ctexart")
      (setq org-latex-pdf-process '("xelatex -interaction nonstopmode -output-directory %o %f"
                                    "xelatex -interaction nonstopmode -output-directory %o %f"
                                    "xelatex -interaction nonstopmode -output-directory %o %f"
                                    "rm -fr %b.out %b.log %b.tex auto"))
      (setq org-latex-listings t)
      (defun org-random-entry (&optional arg)
        "Select and goto a random todo item from the global agenda"
        (interactive "P")
        (if org-agenda-overriding-arguments
            (setq arg org-agenda-overriding-arguments))
        (if (and (stringp arg)
                 (not (string-match "\\S-" arg)))
            (setq arg nil))
        (let* ((today (org-today))
               (date (calendar-gregorian-from-absolute today))
               (kwds org-todo-keywords-for-agenda)
               (lucky-entry nil)
               (completion-ignore-case t)
               (org-agenda-buffer (when (buffer-live-p org-agenda-buffer)
                                    org-agenda-buffer))
               (org-select-this-todo-keyword (if (stringp arg)
                                                 arg
                                               (and arg
                                                    (integerp arg)
                                                    (> arg 0)
                                                    (nth (1- arg)
                                                         kwds))))
               rtn
               rtnall
               files
               file
               pos
               marker
               buffer)
          (when (equal arg '(4))
            (setq org-select-this-todo-keyword (org-icompleting-read "Keyword (or KWD1|K2D2|...): "
                                                                     (mapcar 'list kwds)
                                                                     nil
                                                                     nil)))
          (and (equal 0 arg)
               (setq org-select-this-todo-keyword nil))
          (catch 'exit
            (org-compile-prefix-format 'todo)
            (org-set-sorting-strategy 'todo)
            (setq files (org-agenda-files nil 'ifmode)
                  rtnall
                  nil)
            (while (setq file (pop files))
              (catch 'nextfile
                (org-check-agenda-file file)
                (setq rtn (org-agenda-get-day-entries file date :todo))
                (setq rtnall (append rtnall rtn))))
            (when rtnall
              (setq lucky-entry (nth (random (safe-length (setq entries rtnall)))
                                     entries))
              (setq marker (or (get-text-property 0 'org-marker lucky-entry)
                               (org-agenda-error)))
              (setq buffer (marker-buffer marker))
              (setq pos (marker-position marker))
              (org-pop-to-buffer-same-window buffer)
              (widen)
              (goto-char pos)
              (when (derived-mode-p 'org-mode)
                (org-show-context 'agenda)
                (save-excursion
                  (and (outline-next-heading)
                       (org-flag-heading nil))) ; show the next heading
                (when (outline-invisible-p)
                  (show-entry)) ; display invisible text
                (run-hooks 'org-agenda-after-show-hook))))))
      ;;reset subtask
      (setq org-default-properties (cons "RESET_SUBTASKS" org-default-properties))
      ;; (add-hook 'org-after-todo-state-change-hook 'org-subtask-reset)
      (setq org-plantuml-jar-path (expand-file-name "~/.spacemacs.d/plantuml.jar"))
      (setq org-ditaa-jar-path "~/.spacemacs.d/ditaa.jar")
      (org-babel-do-load-languages 'org-babel-load-languages
                                   '((perl . t)
                                     (ruby . t)
                                     (shell . t)
                                     (dot . t)
                                     (js . t)
                                     (latex .t)
                                     (python . t)
                                     (emacs-lisp . t)
                                     (plantuml . t)
                                     (C . t)
                                     (ditaa . t)))
      (require 'ox-md nil t)
      ;; copy from chinese layer
      (defadvice org-html-paragraph
          (before org-html-paragraph-advice
                  (paragraph contents info)
                  activate)
        "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
        (let* ((origin-contents (ad-get-arg 1))
               (fix-regexp "[[:multibyte:]]")
               (fixed-contents (replace-regexp-in-string (concat "\\(" fix-regexp "\\) *\n *\\(" fix-regexp
                                                                 "\\)")
                                                         "\\1\\2"
                                                         origin-contents)))
          (ad-set-arg 1 fixed-contents)))
      ;; define the refile targets

      (setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
      (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
      (setq org-agenda-file-work (expand-file-name "work.org" org-agenda-dir))
      (setq org-agenda-file-ftc (expand-file-name "fintech.org" org-agenda-dir))
      (setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
      (setq org-agenda-file-code-snippet (expand-file-name "snippet.org" deft-directory))
      (setq org-agenda-file-primary (expand-file-name "primary.org" org-agenda-dir))
      (setq org-agenda-file-pomodora (expand-file-name "pomodora.org" org-agenda-dir))
      (setq org-agenda-file-gmail (expand-file-name "gmail.org" org-agenda-dir))
      (setq org-agenda-files (list org-agenda-dir))
      (with-eval-after-load 'org-agenda
        (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro)
        (spacemacs/set-leader-keys-for-major-mode
          'org-agenda-mode "." 'spacemacs/org-agenda-transient-state/body))
      ;; the %i would copy the selected text into the template
      ;;http://www.howardism.org/Technical/Emacs/journaling-org.html
      ;;add multi-file journal
      (setq org-capture-templates '(("t" "Todo"
                                     entry
                                     (file+headline org-agenda-file-gtd "ToDo")
                                     "* TODO [#B] %?\n  %i\n  %U"
                                     :empty-lines 1)
                                    ("s" "Study"
                                     entry
                                     (file+headline org-agenda-file-gtd "StudyToDo")
                                     "* TODO [#B] %?\n  %i\n %U"
                                     :empty-lines 1)
                                    ("l" "Link File"
                                     entry
                                     (file+headline org-agenda-file-work "WorkToDo")
                                     "* TODO [#A] %?  :@WORK:\n  %i\n %a \n  %U"
                                     :empty-lines 1)
                                    ("w" "Work"
                                     entry
                                     (file+headline org-agenda-file-work "WorkToDo")
                                     "* TODO [#A] %? :@WORK:\n  %i\n  %U"
                                     :empty-lines 1)
                                    ("f" "Fintech"
                                     entry
                                     (file+headline org-agenda-file-ftc "FtcToDo")
                                     "* TODO [#A] %? :@FTC:\n  %i\n  %U"
                                     :empty-lines 1)
                                    ("c" "Chrome"
                                     entry
                                     (file+headline org-agenda-file-note "Link")
                                     "* TODO [#C] %?\n %(I/retrieve-chrome-current-tab-url)\n %i\n %U"
                                     :empty-lines 1)
                                    ("B" "Brave"
                                     entry
                                     (file+headline org-agenda-file-note "Link")
                                     "* TODO [#C] %?\n %(I/retrieve-brave-current-tab-url)\n %i\n %U"
                                     :empty-lines 1)
                                    ("n" "Notes"
                                     entry
                                     (file+headline org-agenda-file-note "Notes")
                                     "* %?\n  %i\n  %U"
                                     :empty-lines 1)
                                    ("o" "Code Snippet"
                                     entry
                                     (file org-agenda-file-code-snippet)
                                     "* %?\t%^g\n#+BEGIN_SRC %^{language}\n#+END_SRC")
                                    ("i" "Ideas"
                                     entry
                                     (file+headline org-agenda-file-note "Ideas")
                                     "* TODO [#C] %?\t%^g\n  %i\n  %U"
                                     :empty-lines 1)
                                    ("b" "Primary Brave"
                                     entry
                                     (file+headline org-agenda-file-primary "DO IT")
                                     "* TODO [#A] %?     \n %(I/retrieve-brave-current-tab-url)\n %i\n %U"
                                     :empty-lines 1)
                                    ("p" "Primary"
                                     entry
                                     (file+headline org-agenda-file-primary "DO IT")
                                     "* TODO [#A] %?     \n  %i\n %U"
                                     :empty-lines 1)
                                    ("P" "Pomodora"
                                     entry
                                     (file+headline org-agenda-file-pomodora "Pomodora")
                                     "* %?    :POMO:\n  %i\n "
                                     :empty-lines 1)
                                    ("e" "event"
                                     entry
                                     (file+headline org-agenda-file-gmail "Gmail Work Event")
                                     "* %?    :GMAIL:\n  %i\n "
                                     :empty-lines 1)
                                    ("j" "Journal Entry"
                                     entry
                                     ;; (file+datetree org-agenda-file-journal)
                                     (file+olp+datetree org-agenda-file-journal)
                                     ;; "* TODO %(format-time-string \"%H:%M\") %?"
                                     "* TODO %?"
                                     :empty-lines 1)))
      (setq org-tag-alist '((:startgroup . nil)
                            ("@PROJECT" . ?j)
                            ("@WORK" . ?w)
                            ("@FTC" . ?f)
                            ("@IDEA" . ?i)
                            ("@STUDY" . ?s)
                            ("@SNIPPET" . ?o)
                            (:endgroup . nil)
                            ("TOC" . ?t)
                            ("ALGO" . ?a)
                            ("ML" . ?m)
                            ("KG" . ?k)
                            ("CS" . ?d)
                            ("LINK" . ?l)
                            ("CRYPT" . ?c)
                            ("PRIMARY" . ?p)
                            ("POMO" . ?b)
                            ))
      ;;An entry without a cookie is treated just like priority ' B '.
      ;;So when create new task, they are default
      (setq org-agenda-custom-commands '(
                                         ("w" "Work" tags-todo "@WORK")
                                         ("g" . "Work subtitle")
                                         ("gf" "FLOW" tags-todo "FLOW")
                                         ("ga" "important&emergency" tags-todo "+PRIORITY=\"A\"")
                                         ("gb" "important&not-emergency" tags-todo
                                          "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
                                         ("gc" "not-important&not-emergency" tags-todo
                                          "+PRIORITY=\"C\"")
                                         ("f" "Ftc" tags-todo "@FTC")
                                         ("i" "Idea" tags-todo "@IDEA")
                                         ("y" "Study" tags-todo "@STUDY")
                                         ("l" "Link" tags-todo "LINK")
                                         ("p" "Snippet" tags-todo "@SNIPPET")
                                         ("j" "Project" tags-todo "@PROJECT")
                                         ;; ("p" . "")
                                         ;; ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"TODOW\"")
                                         ;; ("pl" tags-todo "PROJECT+DREAM+CATEGORY=\"TODOL\"")
                                         ("W" "Weekly Review"
                                          ((stuck "") ;; review stuck projects as designated by org-stuck-projects
                                           (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
                                           ))))
      (defvar I-website-html-preamble "<div class='nav'>
<ul>
<li><a href='http://I.com'>博客</a></li>
<li><a href='/index.html'>Wiki 目录</a></li>
</ul>
</div>")
      (defvar I-website-html-blog-head " <link rel='stylesheet' href='css/site.css' type='text/css'/> \n
    <link rel=\"stylesheet\" type=\"text/css\" href=\"/css/worg.css\"/>")
      (setq org-publish-project-alist `(("blog-notes" :base-directory "~/org-notes"
                                         :base-extension "org"
                                         :publishing-directory "~/org-notes/public_html/"
                                         :recursive t
                                         :html-head ,I-website-html-blog-head
                                         :publishing-function org-html-publish-to-html
                                         :headline-levels 4
                                         :auto-preamble t
                                         :exclude "gtd.org"
                                         :exclude-tags ("ol" "noexport"):section-numbers
                                         nil
                                         :html-preamble ,I-website-html-preamble
                                         :author "kay"
                                         :auto-sitemap t
                                         :sitemap-filename "index.org"
                                         :sitemap-title "我的 wiki"
                                         :sitemap-sort-files anti-chronologically
                                         :sitemap-file-entry-format "%t")
                                        ("blog-static" :base-directory "~/org-notes"
                                         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
                                         :publishing-directory "~/org-notes/public_html/"
                                         :recursive t
                                         :publishing-function org-publish-attachment)
                                        ("blog" :components ("blog-notes" "blog-static"))))
      (add-hook 'org-after-todo-statistics-hook
                'I/org-summary-todo)
      ;; used by I/org-clock-sum-today-by-tags
      (define-key org-mode-map (kbd "s-p") 'org-priority)
      (spacemacs/set-leader-keys-for-major-mode
        'org-mode "tl" 'org-toggle-link-display)
      (define-key evil-normal-state-map (kbd "C-c C-w") 'org-refile)
      ;; hack for org headline toc
      (defun org-html-headline (headline contents info)
        "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
        (unless (org-element-property :footnote-section-p headline)
          (let* ((numberedp (org-export-numbered-headline-p headline info))
                 (numbers (org-export-get-headline-number headline info))
                 (section-number (and numbers
                                      (mapconcat #'number-to-string numbers "-")))
                 (level (+ (org-export-get-relative-level headline info)
                           (1- (plist-get info :html-toplevel-hlevel))))
                 (todo (and (plist-get info :with-todo-keywords)
                            (let ((todo (org-element-property :todo-keyword headline)))
                              (and todo
                                   (org-export-data todo info)))))
                 (todo-type (and todo
                                 (org-element-property :todo-type headline)))
                 (priority (and (plist-get info :with-priority)
                                (org-element-property :priority headline)))
                 (text (org-export-data (org-element-property :title headline)
                                        info))
                 (tags (and (plist-get info :with-tags)
                            (org-export-get-tags headline info)))
                 (full-text (funcall (plist-get info :html-format-headline-function)
                                     todo
                                     todo-type
                                     priority
                                     text
                                     tags
                                     info))
                 (contents (or contents ""))
                 (ids (delq nil
                            (list (org-element-property :CUSTOM_ID headline)
                                  (org-export-get-reference headline info)
                                  (org-element-property :ID headline))))
                 (preferred-id (car ids))
                 (extra-ids (mapconcat (lambda (id)
                                         (org-html--anchor (if (org-uuidgen-p id)
                                                               (concat "ID-" id)
                                                             id)
                                                           nil
                                                           nil
                                                           info))
                                       (cdr ids)
                                       "")))
            (if (org-export-low-level-p headline info)
                ;; This is a deep sub-tree: export it as a list item.
                (let* ((type (if numberedp 'ordered 'unordered))
                       (itemized-body (org-html-format-list-item contents
                                                                 type
                                                                 nil
                                                                 info
                                                                 nil
                                                                 (concat (org-html--anchor preferred-id nil nil info)
                                                                         extra-ids
                                                                         full-text))))
                  (concat (and (org-export-first-sibling-p headline info)
                               (org-html-begin-plain-list type))
                          itemized-body
                          (and (org-export-last-sibling-p headline info)
                               (org-html-end-plain-list type))))
              (let ((extra-class (org-element-property :HTML_CONTAINER_CLASS headline))
                    (first-content (car (org-element-contents headline))))
                ;; Standard headline.  Export it as a section.
                (format "<%s id=\"%s\" class=\"%s\">%s%s</%s>\n"
                        (org-html--container headline info)
                        (org-export-get-reference headline info)
                        (concat (format "outline-%d" level)
                                (and extra-class " ")
                                extra-class)
                        (format "\n<h%d id=\"%s\">%s%s</h%d>\n"
                                level
                                preferred-id
                                extra-ids
                                (concat (and numberedp
                                             (format "<span class=\"section-number-%d\">%s</span> "
                                                     level
                                                     (mapconcat #'number-to-string numbers ".")))
                                        full-text)
                                level)
                        ;; When there is no section, pretend there is an
                        ;; empty one to get the correct <div
                        ;; class="outline-...> which is needed by
                        ;; `org-info.js'.
                        (if (eq (org-element-type first-content) 'section)
                            contents
                          (concat (org-html-section first-content "" info)
                                  contents))
                        (org-html--container headline info))))))))))

(defun I-org/init-org-mac-link ()
  (use-package org-mac-link
    :commands org-mac-grab-link
    :init (progn
            (add-hook 'org-mode-hook
                      (lambda ()
                        (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))):defer
    t))


(defun I-org/init-org-tree-slide ()
  (use-package org-tree-slide
    :init (spacemacs/set-leader-keys "oto" 'org-tree-slide-mode)))


(defun I-org/init-org-download ()
  (use-package org-download
    :defer t
    :init (org-download-enable)))

(defun I-org/init-plain-org-wiki ()
  (use-package plain-org-wiki
    :init (setq pow-directory "~/org-notes")))

(defun I-org/init-worf ()
  (use-package worf
    :defer t
    :init (add-hook 'org-mode-hook 'worf-mode)))

(defun I-org/post-init-deft ()
  (progn
    (setq deft-use-filter-string-for-filename
          t)
    (setq deft-recursive t)
    (setq deft-extension "org")
    (setq deft-directory deft-dir)))

;; (defun I-org/init-sound-wav ()
;;   (use-package sound-wav
;;     :defer t
;;     :init))
;;; packages.el ends here

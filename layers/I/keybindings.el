;;; keybindings.el --- I Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 I
;;
;; URL: https://github.com/I/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; clean bingding key for rebingding
(spacemacs/set-leader-keys
  "bm" nil)
(spacemacs/set-leader-keys-for-major-mode 'org-mode "d" nil)
;; declare prefix
(spacemacs/declare-prefix "." "cheat.sh")
(spacemacs/declare-prefix "o" "self-define")
(spacemacs/declare-prefix "oj" "journal")
(spacemacs/declare-prefix "ol" "layout")
(spacemacs/declare-prefix "op" "clocks")
(spacemacs/declare-prefix "ot" "Toggle")
;; (spacemacs/declare-prefix "os" "Search")
(spacemacs/declare-prefix "bm" "Bookmark")
;; (spacemacs/declare-prefix "xy" "Youdao")

;; For org mode
(spacemacs/declare-prefix-for-mode 'org-mode "mo" "self-define(org-mode)")
(spacemacs/declare-prefix-for-mode 'org-mode "mh" "heading")
(spacemacs/declare-prefix-for-mode 'org-mode "mor" "org-ref")
(spacemacs/declare-prefix-for-mode 'org-mode "mj" "jira")
(spacemacs/declare-prefix-for-mode 'org-mode "mjc" "comments")
(spacemacs/declare-prefix-for-mode 'org-mode "mjp" "projects")
(spacemacs/declare-prefix-for-mode 'org-mode "mji" "issues")
(spacemacs/declare-prefix-for-mode 'org-mode "mjs" "subtasks")
(spacemacs/declare-prefix-for-mode 'org-mode "mjt" "todos")
(spacemacs/declare-prefix-for-mode 'org-mode "mjw" "worklogs")
(spacemacs/declare-prefix-for-mode 'org-mode "d" "dates")

;; For python mode
(spacemacs/declare-prefix-for-mode 'python-mode "mi" "skeleton(import)")
(spacemacs/declare-prefix-for-mode 'python-mode "mf" "formatter")

;; for youdao dictionay mode
(spacemacs/declare-prefix-for-mode 'youdao-dictionary-mode "g" "google-translate")

;; for cheat.sh  https://github.com/chubin/cheat.sh
(spacemacs/set-leader-keys
  ".f" 'cheat-sh-search-topic
  ".r" 'cheat-sh-maybe-region
  ".s" 'cheat-sh
  ".c" 'cheat-sh-search
  ".h" 'cheat-sh-help
  ".l" 'cheat-sh-list
  )

(spacemacs/set-leader-keys
  "sW" 'engine/search-google
  "sw" 'helm-google-suggest

  "ojj" 'org-journal-new-entry
  "oja" 'org-journal-new-scheduled-entry
  "ojs" 'org-journal-search-forever

  "opp" 'org-pomodoro
  "opc" 'org-clock-cancel
  "opg" 'org-clock-goto
  "opi" 'org-clock-in
  "opI" 'org-clock-in-last
  "opj" 'spacemacs/org-clock-jump-to-current-clock
  "opo" 'org-clock-out
  "opr" 'org-resolve-clocks
  "ope" 'org-pomodoro-extend-last-clock
  )

(spacemacs/set-leader-keys
  "oc" 'org-capture
  "om" 'org-pomodoro
  "pn" 'treemacs-projectile

  "fF" 'I/find-file-remote

  "pa" 'projectile-find-other-file
  "pA" 'projectile-find-other-file-other-window
  ":" 'counsel-M-x

  "bms" 'bookmark-set
  "bmr" 'bookmark-rename
  "bmd" 'bookmark-delete
  "bmj" 'counsel-bookmark

  "bj" 'spacemacs/new-empty-buffer-below

  "ca" 'comment-dwim
  "cA" 'comment-kill

  "oo" 'I/helm-hotspots
  "ef" 'flycheck-mode
  "ed" 'flycheck-disable-checker
  "en" 'flycheck-next-error
  "ep" 'flycheck-previous-error

  ;; insert
  "ib" 'insert-buffer
  "if" 'insert-file

  "ow" 'eww

  "od" 'I/occur-dwim
  "ok" 'I-kill-other-persp-buffers
  "ox" 'org-open-at-point-global
  ;; "or" 'I/browser-refresh--chrome-applescript
  "or" 'youdao-dictionary-play-voice-at-point
  "Fr" 'I/Rename-File-And-Buffer
  "bD" 'spacemacs/kill-other-buffers

  ;; hightlight
  "hh" 'I/highlight-dwim
  "hc" 'I/clearn-highlight


  "rh" 'helm-resume
  "sj" 'counsel-imenu
  "tt" 'tooltip-mode
  "tI" 'highlight-indentation-mode
  "tY" 'yapf-mode

  "o(" 'ielm

  "gL" 'magit-log-buffer-file
  "og" 'my-git-timemachine
  "sj" 'I/counsel-imenu
  "os" 'I/sql-connect-server

  "fl" 'find-file-literally-at-point
  "ri" 'ivy-resume
  "fh" 'ffap-hexl-mode
  "fd" 'projectile-find-file-dwim-other-window
  "nl" 'spacemacs/evil-search-clear-highlight
  "oll" 'I/load-my-layout
  "ols" 'I/save-my-layout
  ;; "ob" 'popwin:display-last-buffer
  ;; "ob" 'xwidget-webkit-browse-url
  "ob" 'xwwp
  "oY" 'youdao-dictionary-search-at-point+
  "oy" 'youdao-dictionary-search-at-point

  "bM" 'spacemacs/switch-to-messages-buffer
  "sS" 'spacemacs/swiper-region-or-symbol

  "o'" 'ansi-term

  "a." 'server-start
  "ac" 'org-capture
  "aC" 'calc-dispatch
  "aa" 'org-agenda
  )

(spacemacs/set-leader-keys
  "xC" 'I/capitalize-dwim
)

;; for fix waring
(spacemacs/set-leader-keys "mwD" 'dired-do-chgrp)
(spacemacs/set-leader-keys "mwN" 'org-agenda-next-line)
(spacemacs/set-leader-keys "mwG" 'org-agenda-toggle-time-grid)
(spacemacs/set-leader-keys "mw|" 'org-agenda-filter-remove-all)


;; ;; For didtionary
;; (spacemacs/set-leader-keys
;;   ;; youdao
;;   "xyq" 'youdao-dictionary-search-from-input
;;   "xyt" 'youdao-dictionary-search-at-point
;;   "xyd" 'youdao-dictionary-search-at-point-tooltip
;;   "xyy" 'youdao-dictionary-search-at-point+
;;   "xys" 'youdao-dictionary-play-voice-at-point
;;   "xyw" 'youdao-dictionary-play-voice-of-current-word

;;   ;; for osx dictionary
;;   "xyo" 'osx-dictionary-search-pointer
;;   )


;; for emacs toggle latex
(spacemacs/set-leader-keys
  "otl" 'org-toggle-latex-fragment
  "oti" 'I/toggle-indent-guide-recursive
  "otc" 'org-cdlatex-mode
  "oi" 'google-translate-at-point
  "od" 'osx-dictionary-search-pointer
  "oq" 'youdao-dictionary-search-from-input
  )

;; ;; Replace this in ~/.emacs.d/*/osx-dictionary-*/osx-dictionary.el
;; (defvar osx-dictionary-mode-header-line
;;   '(
;;     (:propertize "o" face mode-line-buffer-id)
;;     ": gg translate"
;;     "    "
;;     (:propertize "t" face mode-line-buffer-id)
;;     ": yd search"
;;     "    "
;;     (:propertize "y" face mode-line-buffer-id)
;;     ": yd search+"
;;     "    "
;;     (:propertize "d" face mode-line-buffer-id)
;;     ": yd tooltip"
;;     "    "

;;     ;; default
;;     (:propertize "s" face mode-line-buffer-id)
;;     ": Search Word"
;;     "    "
;;     (:propertize "w" face mode-line-buffer-id)
;;     ": Open in Dictionary.app"
;;     "    "
;;     (:propertize "r" face mode-line-buffer-id)
;;     ": Read word"
;;     "    "
;;     (:propertize "q" face mode-line-buffer-id)
;;     ": Quit")
;;   "Header-line used on the `osx-dictionary-mode'.")
;; ;; Replace this in ~/.emacs.d/*/osx-dictionary-*/osx-dictionary.el
;; (defvar osx-dictionary-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     ;; Dictionary commands
;;     (define-key map "o" 'google-translate-at-point)
;;     (define-key map "t" 'youdao-dictionary-search-at-point)
;;     (define-key map "y" 'youdao-dictionary-search-at-point+)
;;     (define-key map "d" 'youdao-dictionary-search-at-point-tooltip)

;;     ;; default
;;     (define-key map "q" 'osx-dictionary-quit)
;;     (define-key map "s" 'osx-dictionary-search-input)
;;     (define-key map "w" 'osx-dictionary-open-dictionary.app)
;;     (define-key map "r" 'osx-dictionary-read-word)
;;     ;; Misc
;;     (define-key map "?" 'describe-mode)
;;     map)
;;   "Keymap for `osx-dictionary-mode'.")

(spacemacs/set-leader-keys-for-major-mode 'xwidget-webkit-mode
  "." 'xwidget-hydra
  "s" 'xwwp-follow-link
  )

(spacemacs/set-leader-keys-for-major-mode 'markdown-mode
  "ta" 'markdown-insert-table
  )

;; For  mode keybing
(spacemacs/set-leader-keys-for-major-mode 'youdao-dictionary-mode
  "q" 'youdao-dictionary-search-from-input
  "t" 'youdao-dictionary-search-at-point
  "y" 'youdao-dictionary-search-at-point+
  "s" 'youdao-dictionary-play-voice-at-point
  "w" 'youdao-dictionary-play-voice-of-current-word

  ;; for osx dictionary
  "o" 'osx-dictionary-search-pointer

  ;; for google translate
  "gl" 'spacemacs/set-google-translate-languages
  "gQ" 'google-translate-query-translate-reverse
  "gq" 'google-translate-query-translate
  "gT" 'google-translate-at-point-reverse
  "gt" 'google-translate-at-point
  )

;; For youdao mode keybing
(spacemacs/set-leader-keys-for-major-mode 'youdao-dictionary-mode
  "q" 'youdao-dictionary-search-from-input
  "t" 'youdao-dictionary-search-at-point
  "y" 'youdao-dictionary-search-at-point+
  "s" 'youdao-dictionary-play-voice-at-point
  "w" 'youdao-dictionary-play-voice-of-current-word

  ;; for osx dictionary
  "o" 'osx-dictionary-search-pointer
  "r" 'osx-dictionary-read-word

  ;; for google translate
  "gl" 'spacemacs/set-google-translate-languages
  "gQ" 'google-translate-query-translate-reverse
  "gq" 'google-translate-query-translate
  "gT" 'google-translate-at-point-reverse
  "gt" 'google-translate-at-point
  )

;; For org-mode jira
(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "jpg" 'org-jira-get-projects
  "jib" 'org-jira-browse-issue
  "jig" 'org-jira-get-issues
  "jij" 'org-jira-get-issues-from-custom-jql
  "jih" 'org-jira-get-issues-headonly
  "jiu" 'org-jira-update-issue
  "jiw" 'org-jira-progress-issue
  "jin" 'org-jira-progress-issue-next
  "jia" 'org-jira-assign-issue
  "jir" 'org-jira-refresh-issue
  "jiR" 'org-jira-refresh-issues-in-buffer
  "jic" 'org-jira-create-issue
  "jik" 'org-jira-copy-current-issue-key
  "jsc" 'org-jira-create-subtask
  "jsg" 'org-jira-get-subtasks
  "jcc" 'org-jira-add-comment
  "jcu" 'org-jira-update-comment
  "jwu" 'org-jira-update-worklogs-from-org-clocks
  "jtj" 'org-jira-todo-to-jira
  "jif" 'org-jira-get-issues-by-fixversion
  )

;; For org-mode
(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "tte" 'org-table-toggle-column-width
  "hu" 'outline-up-heading
  "hh" 'org-backward-heading-same-level
  "hk" 'org-previous-visible-heading
  "hj" 'org-next-visible-heading
  "hl" 'org-forward-heading-same-level

  "ig" 'org-insert-structure-template
  "id" 'I/org-insert-src-block
  "ttc" 'org-cdlatex-mode
  "ttl" 'org-toggle-latex-fragment
  "iC" 'org-ref-helm-insert-cite-link
  "ib" 'I/insert-brave-current-tab-url
  "iB" 'org-insert-structure-template
  "ic" 'I/capture-screenshot
  "oc" 'I/capitalize-dwim
  "ol" 'org-toggle-latex-fragment

  "oa" 'I/org-archive-tasks
  ;; For encrypt and decrypt
  "oe" 'org-encrypt-entrys
  "od" 'org-decrypt-entry
  "oD" 'org-decrypt-entrys
  ;; For org-ref
  "ori" 'org-ref-insert-cite-link
  "orc" 'org-ref-helm-insert-cite-link

  ;; for insert date time
  "dn" 'I/now
  "d." 'I/now-timestamp
  "do" 'I/now-today
  "di" 'I/now-time
  "dm" 'I/now-hour-min
  )

;; For xwidget webkit mode (browser)
(spacemacs/set-leader-keys-for-major-mode 'xwidget-webkit-mode
  "f" 'xwidget-webkit-forward
  "b" 'xwidget-webkit-back
  "c" 'xwidget-cleanup
  "g" 'xwidget-webkit-browse-url
  "i" 'xwidget-webkit-zoom-in
  "o" 'xwidget-webkit-zoom-out
  "h" 'xwidget-webkit-cx2
  "v" 'xwidget-webkit-cx3
  "r" 'xwidget-webkit-reload
  "d" 'xwidget-webkit-scroll-up
  "u" 'xwidget-webkit-scroll-down
  "t" 'xwidget-webkit-scroll-top
  "e" 'xwidget-webkit-scroll-bottom
  )

;; For python-mode
(spacemacs/set-leader-keys-for-major-mode 'python-mode
  "fb" 'yapfify-buffer
  "fr" 'yapfify-region
  "ff" 'blacken-buffer

  "ii" 'python-skeleton-import
  "ic" 'python-skeleton-class
  "ri" 'I/py-optimize-imports
  "rI" 'spacemacs/python-remove-unused-imports
  "rs" 'py-isort-buffer

  "hd" 'anaconda-mode-show-doc
  )

(define-key global-map (kbd "s-i") 'yas/insert-snippet)
;; (define-key evil-insert-state-map "M-i" 'yas/insert-snippet)

;; A complementary binding to the apropos-command (C-h a)
(define-key 'help-command "A" 'apropos)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-i") 'info-display-manual)
(define-key global-map (kbd "C-c d") 'youdao-dictionary-search-at-point+)
(define-key global-map (kbd "C-c s") 'google-translate-at-point)
(define-key global-map (kbd "C-c y") 'osx-dictionary-search-pointer)
(define-key global-map (kbd "<f9>") 'org-capture)
(define-key global-map (kbd "C-c t") 'org-capture)
(define-key global-map (kbd "<f8>") 'I/show-current-buffer-major-mode)
(define-key evil-normal-state-map "zi" 'hs-toggle-hiding)
(define-key evil-normal-state-map "zI" 'I/hs-hide-leaves)
(define-key evil-normal-state-map "zS" 'hs-show-all)
(define-key evil-normal-state-map (kbd "go") 'evil-jump-backward)
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
(define-key global-map (kbd "C-c c") 'I/capture-screenshot)
;; (define-key evil-insert-state-map "\C-e" 'end-of-line)
;; (define-key 'ivy-occur-grep-mode-map (kbd "C-d") 'evil-scroll-down)
;; http://emacs.stackexchange.com/questions/220/how-to-bind-c-i-as-different-from-tab
;; (define-key input-decode-map [?\C-i] [C-i])
;; (define-key evil-normal-state-map (kbd "C-i") 'evil-jump-forward)

;; drag and open from item - mac
(global-set-key [s-drag-n-drop] 'ns-drag-n-drop-as-text)

(global-set-key [(shift return)] 'I/smart-open-line)
(global-set-key (kbd "s-/") 'hippie-expand)
(global-set-key (kbd "C-c a") 'org-agenda)

(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c i e") 'spacemacs/auto-yasnippet-expand)
(global-set-key (kbd "<f1>") 'I/helm-hotspots)
(global-set-key (kbd "C-M-\\") 'I/indent-region-or-buffer)
(global-set-key [remap fill-paragraph] #'endless/fill-or-unfill)
(global-set-key (kbd "C-s-h") 'mark-defun)
(global-set-key (kbd "s-l") 'goto-line)
;; (global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "C-`") 'toggle-input-method)
(global-set-key (kbd "s-d") 'I/my-mc-mark-next-like-this)
(global-set-key (kbd "<f5>") 'I/run-current-file)
(global-set-key (kbd "s-s") 'save-buffer)

;; set scroll using touchpad
(global-set-key [wheel-right] 'scroll-left)
(global-set-key [wheel-left] 'scroll-right)


(bind-key* "s-r" 'mc/reverse-regions)
(bind-key* "C-c /" 'company-files)
(bind-key* "s-;" 'I/insert-semicolon-at-the-end-of-this-line)
(bind-key* "C-s-;" 'I/delete-semicolon-at-the-end-of-this-line)
(bind-key* "s-," 'I/insert-comma-at-the-end-of-this-line)
;; (bind-key* "C-s-," 'I/delete-comma-at-the-end-of-this-line)
(bind-key* "C-c L" 'org-cliplink)
(bind-key* "C-c l" 'I/insert-brave-current-tab-url)
(bind-key* "C-=" 'er/expand-region)
(bind-key* "M--" 'I/goto-match-paren)
(bind-key* "C-c k" 'which-key-show-top-level)
(bind-key* "s-y" 'aya-expand)
(bind-key* "C-." 'I/insert-space-after-point)
(bind-key* "M-i" 'string-inflection-java-style-cycle)
(bind-key* "M-u" 'I/upcase-dwim)
(bind-key* "M-l" 'I/downcase-dwim)
(bind-key* "M-c" 'I/capitalize-dwim)
(bind-key* "s-p" 'find-file-in-project)
;; (bind-key* "C-l" 'recenter)
;; (bind-key* ">" 'I/tab-region)
;; (bind-key* "<" 'I/untab-region)
(define-key evil-normal-state-map ">" 'I/tab-region)
(define-key evil-normal-state-map "<" 'I/untab-region)

;; Auto add blank after comma
(global-set-key (kbd ",")
                #'(lambda ()
                    (interactive)
                    (insert ", ")))

;; ivy specific keybindings
(if (configuration-layer/layer-usedp 'ivy)
    (progn
      (spacemacs/set-leader-keys "ff" 'counsel-find-file)
      (spacemacs/set-leader-keys "fL" 'counsel-locate)
      (spacemacs/set-leader-keys "hi" 'counsel-info-lookup-symbol)
      (spacemacs/set-leader-keys "pb" 'projectile-switch-to-buffer)))

;; for quick open finder and shell
(when (spacemacs/system-is-mac)
  (spacemacs/set-leader-keys "of" 'reveal-in-osx-finder)
  (spacemacs/set-leader-keys "o;" 'I/iterm-shell-cmd)
  (spacemacs/set-leader-keys "o:" 'I/iterm-shell-cmd-new-tab)
  )

(spacemacs|add-toggle toggle-shadowsocks-proxy-mode
  :status shadowsocks-proxy-mode
  :on (global-shadowsocks-proxy-mode)
  :off (global-shadowsocks-proxy-mode -1)
  :documentation "Toggle shadowsocks proxy mode."
  :evil-leader "ots")

;; Utility functions
(defun bb/define-key (keymap &rest bindings)
  (declare (indent 1))
  (while bindings
    (define-key keymap (pop bindings) (pop bindings))))

(with-eval-after-load 'company
  (progn
    (bb/define-key company-active-map
                   (kbd "C-w") 'evil-delete-backward-word)

    (bb/define-key company-active-map
                   (kbd "s-w") 'company-show-location)))

(bb/define-key ivy-occur-grep-mode-map
               (kbd "C-d") 'evil-scroll-down
               "d" 'ivy-occur-delete-candidate)

(bb/define-key evil-normal-state-map
               "+" 'evil-numbers/inc-at-pt
               "-" 'evil-numbers/dec-at-pt
               "\\" 'evil-repeat-find-char-reverse
               (kbd "DEL") 'evil-repeat-find-char-reverse
               "[s" (lambda (n) (interactive "p") (dotimes (c n nil) (insert " ")))
               "]s" (lambda (n) (interactive "p")
                      (forward-char) (dotimes (c n nil) (insert " ")) (backward-char (1+ n))))

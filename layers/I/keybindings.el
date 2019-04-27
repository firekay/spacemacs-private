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
;; declare prefix
(spacemacs/declare-prefix "o" "self-define")
(spacemacs/declare-prefix-for-mode 'org-mode "mo" "self-define(org-mode)")
(spacemacs/declare-prefix-for-mode 'org-mode "mor" "org-ref")
(spacemacs/declare-prefix-for-mode 'python-mode "mi" "skeleton(import)")
(spacemacs/declare-prefix-for-mode 'python-mode "mo" "self_define")
(spacemacs/declare-prefix "oj" "journal")
(spacemacs/declare-prefix "ot" "Toggle")
(spacemacs/declare-prefix "os" "Search")
(spacemacs/declare-prefix "bm" "Bookmark")
(spacemacs/declare-prefix "xy" "Youdao")

;; Auto add blank after comma
(global-set-key (kbd ",")
                #'(lambda ()
                    (interactive)
                    (insert ", ")))

(spacemacs/set-leader-keys "pn" 'treemacs-projectile)
(define-key global-map (kbd "C-c c") 'I/capture-screenshot)

(define-key evil-normal-state-map (kbd "go") 'evil-jump-backward)

;;; For set leader keys
(spacemacs/set-leader-keys "tI" 'highlight-indentation-mode)
(spacemacs/set-leader-keys "tY" 'yapf-mode)

(spacemacs/set-leader-keys
  "ojj" 'org-journal-new-entry
  "oja" 'org-journal-new-scheduled-entry
  "ojs" 'org-journal-search-forever
  )

(spacemacs/set-leader-keys
  "ca" 'comment-dwim
  "cA" 'comment-kill
  )

(spacemacs/set-leader-keys "tt" 'tooltip-mode)

(spacemacs/set-leader-keys "bms" 'bookmark-set)
(spacemacs/set-leader-keys "bmr" 'bookmark-rename)
(spacemacs/set-leader-keys "bmd" 'bookmark-delete)
(spacemacs/set-leader-keys "bmj" 'counsel-bookmark)

(spacemacs/set-leader-keys "od" 'occur-dwim)
(spacemacs/set-leader-keys "ok" 'I-kill-other-persp-buffers)
(spacemacs/set-leader-keys "ox" 'org-open-at-point-global)
(spacemacs/set-leader-keys "or" 'I/browser-refresh--chrome-applescript)

(spacemacs/set-leader-keys "rh" 'helm-resume)
(spacemacs/set-leader-keys "sj" 'counsel-imenu)

(spacemacs/set-leader-keys
  "o>" 'I/tab-region
  "o<" 'I/untab-region
  "oo" 'I/helm-hotspots
  ;; "oc" 'my-auto-update-tags-when-save
  "op" 'I/org-save-and-export
  "Fr" 'I/Rename-File-And-Buffer
  "bD" 'spacemacs/kill-other-buffers

  "en" 'flycheck-next-error
  "ep" 'flycheck-previous-error
  "o(" 'ielm

  "gL" 'magit-log-buffer-file
  "og" 'my-git-timemachine
  "sj" 'I/counsel-imenu
  "os" 'counsel-ag-thing-at-point

  "fl" 'find-file-literally-at-point
  "ri" 'ivy-resume
  "fh" 'ffap-hexl-mode
  "fd" 'projectile-find-file-dwim-other-window
  "nl" 'spacemacs/evil-search-clear-highlight
  "oll" 'I/load-my-layout
  "ols" 'I/save-my-layout
  "ob" 'popwin:display-last-buffer
  "oy" 'youdao-dictionary-search-at-point+
  "oY" 'youdao-dictionary-search-at-point

  "bM" 'spacemacs/switch-to-messages-buffer
  "sS" 'spacemacs/swiper-region-or-symbol

  "o'" 'ansi-term

  "a." 'server-start
  )

;; for fix waring
(spacemacs/set-leader-keys "mwD" 'dired-do-chgrp)
(spacemacs/set-leader-keys "mwN" 'org-agenda-next-line)
(spacemacs/set-leader-keys "mwG" 'org-agenda-toggle-time-grid)
(spacemacs/set-leader-keys "mw|" 'org-agenda-filter-remove-all)


;; For didtionary
(spacemacs/set-leader-keys
  ;; youdao
  "xyq" 'youdao-dictionary-search
  "xyt" 'youdao-dictionary-search-at-point
  "xyy" 'youdao-dictionary-search-at-point+

  ;; for osx dictionary
  "xyo" 'osx-dictionary-search-pointer
  )


;; for emacs toggle latex
(spacemacs/set-leader-keys
  "otl" 'org-toggle-latex-fragment
  "oti" 'I/toggle-indent-guide-recursive
  "otc" 'org-cdlatex-mode
  "oi" 'google-translate-at-point
  "od" 'osx-dictionary-search-pointer
  )


;; For org-mode
(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "id" 'I/org-insert-src-block
  "ttc" 'org-cdlatex-mode
  "ttl" 'org-toggle-latex-fragment
  "iC" 'org-ref-helm-insert-cite-link
  "ic" 'I/capture-screenshot
  "oc" 'dakra-capitalize-dwim
  "ol" 'org-toggle-latex-fragment
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


;; For python-mode
(spacemacs/set-leader-keys-for-major-mode 'python-mode
  "ii" 'python-skeleton-import
  "ic" 'python-skeleton-class
  "of" 'flycheck-mode
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

;; (define-key 'ivy-occur-grep-mode-map (kbd "C-d") 'evil-scroll-down)

(global-set-key [(shift return)] 'I/smart-open-line)
(global-set-key (kbd "s-/") 'hippie-expand)
(global-set-key (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c d") 'youdao-dictionary-search-at-point+)
(define-key global-map (kbd "C-c s") 'google-translate-at-point)
(define-key global-map (kbd "C-c y") 'osx-dictionary-search-pointer)
(define-key global-map (kbd "<f9>") 'org-capture)
(define-key global-map (kbd "C-c t") 'org-capture)
(define-key global-map (kbd "<f8>") 'I/show-current-buffer-major-mode)

(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c i e") 'spacemacs/auto-yasnippet-expand)
;; http://emacs.stackexchange.com/questions/220/how-to-bind-c-i-as-different-from-tab
;; (define-key input-decode-map [?\C-i] [C-i])
;; (define-key evil-normal-state-map (kbd "C-i") 'evil-jump-forward)
(global-set-key (kbd "C-M-\\") 'I/indent-region-or-buffer)
(global-set-key [remap fill-paragraph] #'endless/fill-or-unfill)

;; (global-set-key (kbd "C-.") 'company-capf)

;; some easy functions for navigate functions
;;C-M-a beginning-of-defun
;;C-M-e end-of-defun
;;C-M-h mark-defun
(global-set-key (kbd "C-s-h") 'mark-defun)

(global-set-key (kbd "s-l") 'goto-line)
;; (global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "C-`") 'toggle-input-method)
(global-set-key (kbd "s-d") 'I/my-mc-mark-next-like-this)
(bind-key* "s-r" 'mc/reverse-regions)
(global-set-key (kbd "<f5>") 'I/run-current-file)

;; "http://endlessparentheses.com/transposing-keybinds-in-emacs.html?source=rss"
;; (global-set-key "\C-t" #'transpose-lines)
;; (define-key ctl-x-map "\C-t" #'transpose-chars)

;; for quick open finder and shell
(when (spacemacs/system-is-mac)
  (spacemacs/set-leader-keys "of" 'reveal-in-osx-finder)
  (spacemacs/set-leader-keys "o;" 'I/iterm-shell-command))

(spacemacs|add-toggle toggle-shadowsocks-proxy-mode
  :status shadowsocks-proxy-mode
  :on (global-shadowsocks-proxy-mode)
  :off (global-shadowsocks-proxy-mode -1)
  :documentation "Toggle shadowsocks proxy mode."
  :evil-leader "ots")

(global-set-key (kbd "s-s") 'save-buffer)
;; (bind-key* "s-k" 'scroll-other-window-down)
;; (bind-key* "s-j"  'scroll-other-window)
(bind-key* "C-c /" 'company-files)
;; (bind-key* "s-r" 'I/browser-refresh--chrome-applescript)
(bind-key* "s-;" 'I/insert-semicolon-at-the-end-of-this-line)
(bind-key* "C-s-;" 'I/delete-semicolon-at-the-end-of-this-line)
(bind-key* "s-," 'I/insert-comma-at-the-end-of-this-line)
;; (bind-key* "C-s-," 'I/delete-comma-at-the-end-of-this-line)
(bind-key* "C-c l" 'I/insert-chrome-current-tab-url)
(bind-key* "C-=" 'er/expand-region)
(bind-key* "M--" 'I/goto-match-paren)
(bind-key* "C-c k" 'which-key-show-top-level)
(bind-key* "s-y" 'aya-expand)
(bind-key* "C-." 'I/insert-space-after-point)
(bind-key* "M-i" 'string-inflection-java-style-cycle)
(bind-key* "M-u" 'dakra-upcase-dwim)
(bind-key* "M-l" 'dakra-downcase-dwim)
(bind-key* "M-c" 'dakra-capitalize-dwim)
;; (bind-key* "C-l" 'recenter)

;; Utility functions
(defun bb/define-key (keymap &rest bindings)
  (declare (indent 1))
  (while bindings
    (define-key keymap (pop bindings) (pop bindings))))

(define-key evil-normal-state-map "zi" 'hs-toggle-hiding)
(define-key evil-normal-state-map "zI" 'hs-hide-leaves)
(define-key evil-normal-state-map "zS" 'hs-show-all)
;; (define-key evil-insert-state-map "\C-e" 'end-of-line)

(bb/define-key evil-normal-state-map
  "+" 'evil-numbers/inc-at-pt
  "-" 'evil-numbers/dec-at-pt
  "\\" 'evil-repeat-find-char-reverse
  (kbd "DEL") 'evil-repeat-find-char-reverse
  "[s" (lambda (n) (interactive "p") (dotimes (c n nil) (insert " ")))
  "]s" (lambda (n) (interactive "p")
         (forward-char) (dotimes (c n nil) (insert " ")) (backward-char (1+ n))))

(bb/define-key ivy-occur-grep-mode-map
  (kbd "C-d") 'evil-scroll-down
  "d" 'ivy-occur-delete-candidate)

(with-eval-after-load 'company
  (progn
    (bb/define-key company-active-map
      (kbd "C-w") 'evil-delete-backward-word)

    (bb/define-key company-active-map
      (kbd "s-w") 'company-show-location)))


(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "<f1>") 'I/helm-hotspots)
;; ivy specific keybindings
(if (configuration-layer/layer-usedp 'ivy)
    (progn
      (spacemacs/set-leader-keys "ff" 'counsel-find-file)
      (spacemacs/set-leader-keys "fL" 'counsel-locate)
      (spacemacs/set-leader-keys "hi" 'counsel-info-lookup-symbol)
      (spacemacs/set-leader-keys "pb" 'projectile-switch-to-buffer)))

(bind-key* "s-p" 'find-file-in-project)
(spacemacs/set-leader-keys "pa" 'projectile-find-other-file)
(spacemacs/set-leader-keys "pA" 'projectile-find-other-file-other-window)
(spacemacs/set-leader-keys ":" 'counsel-M-x)

;; highlight
(spacemacs/set-leader-keys "hh" 'I/highlight-dwim)
(spacemacs/set-leader-keys "hc" 'I/clearn-highlight)

(spacemacs/set-leader-keys-for-major-mode 'python-mode
  "hd" 'anaconda-mode-show-doc
  )

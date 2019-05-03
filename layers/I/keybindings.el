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
(spacemacs/declare-prefix "oj" "journal")
(spacemacs/declare-prefix "ot" "Toggle")
(spacemacs/declare-prefix "os" "Search")
(spacemacs/declare-prefix "bm" "Bookmark")
(spacemacs/declare-prefix "xy" "Youdao")

;; For org mode
(spacemacs/declare-prefix-for-mode 'org-mode "mo" "self-define(org-mode)")
(spacemacs/declare-prefix-for-mode 'org-mode "mor" "org-ref")

;; For python mode
(spacemacs/declare-prefix-for-mode 'python-mode "mi" "skeleton(import)")
(spacemacs/declare-prefix-for-mode 'python-mode "mo" "self_define")
(spacemacs/declare-prefix-for-mode 'python-mode "mf" "formatter")


(spacemacs/set-leader-keys
  "ojj" 'org-journal-new-entry
  "oja" 'org-journal-new-scheduled-entry
  "ojs" 'org-journal-search-forever
  )

(spacemacs/set-leader-keys
  "pn" 'treemacs-projectile

  "pa" 'projectile-find-other-file
  "pA" 'projectile-find-other-file-other-window
  ":" 'counsel-M-x

  "bms" 'bookmark-set
  "bmr" 'bookmark-rename
  "bmd" 'bookmark-delete
  "bmj" 'counsel-bookmark

  "ca" 'comment-dwim
  "cA" 'comment-kill

  "oo" 'I/helm-hotspots
  "oc" 'org-capture
  "oa" 'org-agenda

  "od" 'occur-dwim
  "ok" 'I-kill-other-persp-buffers
  "ox" 'org-open-at-point-global
  "or" 'I/browser-refresh--chrome-applescript
  "op" 'I/org-save-and-export
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
  "xyq" 'youdao-dictionary-search-from-input
  "xyt" 'youdao-dictionary-search-at-point
  "xyy" 'youdao-dictionary-search-at-point+
  "xys" 'youdao-dictionary-play-voice-at-point
  "xyw" 'youdao-dictionary-play-voice-of-current-word

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

  "hd" 'anaconda-mode-show-doc
  "ff" 'blacken-buffer
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
(define-key evil-normal-state-map "zI" 'hs-hide-leaves)
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

(bind-key* "s-r" 'mc/reverse-regions)
(bind-key* "C-c /" 'company-files)
(bind-key* "s-;" 'I/insert-semicolon-at-the-end-of-this-line)
(bind-key* "C-s-;" 'I/delete-semicolon-at-the-end-of-this-line)
(bind-key* "s-," 'I/insert-comma-at-the-end-of-this-line)
;; (bind-key* "C-s-," 'I/delete-comma-at-the-end-of-this-line)
(bind-key* "C-c l" 'I/insert-chrome-current-tab-url)
(bind-key* "C-c L" 'I/insert-brave-current-tab-url)
(bind-key* "C-=" 'er/expand-region)
(bind-key* "M--" 'I/goto-match-paren)
(bind-key* "C-c k" 'which-key-show-top-level)
(bind-key* "s-y" 'aya-expand)
(bind-key* "C-." 'I/insert-space-after-point)
(bind-key* "M-i" 'string-inflection-java-style-cycle)
(bind-key* "M-u" 'dakra-upcase-dwim)
(bind-key* "M-l" 'dakra-downcase-dwim)
(bind-key* "M-c" 'dakra-capitalize-dwim)
(bind-key* "s-p" 'find-file-in-project)
;; (bind-key* "C-l" 'recenter)
(bind-key* ">" 'I/tab-region)
(bind-key* "<" 'I/untab-region)

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
  (spacemacs/set-leader-keys "o;" 'I/iterm-shell-command))

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

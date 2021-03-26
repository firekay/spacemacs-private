;;; config.el --- I Layer packages File for Spacemacs
;;
;; Copyright (c) 2014-2016 I
;;
;; URL: https://github.com/I/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun I/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))

;; (require 'org-gcal)
;; (setq org-gcal-client-id "383904030804-r1kmpl5vn2lh13f00qcqcn0059cclfj5.apps.googleusercontent.com"
;;       org-gcal-fetch-file-alist '(("zhenkai.xu@shopee.com" .  "~/Dropbox/Beorg/GTD/gmail.org")))

(add-hook 'org-mode-hook #'I/org-ispell)

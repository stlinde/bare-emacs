;;; init.el --- -*- lexical-binding: t; coding: utf-8 -*-
;;; Commentary:
;;; Code:

;; Package Repositories
(require 'package)
(add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)

;; No Littering
(unless (package-installed-p 'no-littering)
  (package-install 'no-littering))
(require 'no-littering)


;; Custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Backups
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;; UI
;; Theme
(load-theme 'modus-operandi-deuteranopia :no-confirm)

;; Font
(set-face-attribute 'default nil
                    :family "Iosevka Comfy"
                    :height 105
                    :weight 'semilight)
(set-face-attribute 'variable-pitch nil
                    :family "Iosevka Comfy Motion Duo"
                    :height 105
                    :weight 'semilight)
(set-face-attribute 'fixed-pitch nil
                    :family "Iosevka Comfy"
                    :height 105
                    :weight 'semilight)

;; Modeline
(setq display-time-format " %a %e %b, %H:%M ")
(display-time-mode)
(display-battery-mode)

;; Line-numbers
(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'org-mode-hook 'display-line-numbers-mode))

;; Electric Mode
(add-hook 'after-init-hook #'electric-pair-mode)
(add-hook 'after-init-hook #'electric-indent-mode)

;; Magit
(unless (package-installed-p 'magit)
  (package-install 'magit))
(require 'magit)
(global-set-key (kbd "C-x g") #'magit-status)


;;; init.el ends here

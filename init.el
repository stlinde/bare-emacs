;;; init.el --- -*- lexical-binding: t; coding: utf-8 -*-
;;; Commentary:
;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Packaging Settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Package Repositories
(require 'package)
(add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)


(defun require-package (package)
  (unless (package-installed-p package)
    (package-install package))
  (require package))

;; Place code inside lisp and site-lisp for loading modules
(add-to-list 'load-path (concat user-emacs-directory "site-lisp/"))
(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   General Settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; No Littering
(unless (package-installed-p 'no-littering)
  (package-install 'no-littering))
(require 'no-littering)

;; Setup PATH
(require-package 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setopt auto-revert-avoid-polling t)  ;; Read from disk if file changes
(setopt sentence-end-double-space nil)  ;; Fix archaic defaults

(pixel-scroll-precision-mode)  ;; More precise scrolling

;; Make right-click bring up menu instead of selecting text
(when (display-graphic-p)
  (context-menu-mode))

;; WSL
(defun wsl-copy-region-to-clipboard (start end)
  "Copy region to Windows clipboard."
  (interactive "r")
  (call-process-region start end "clip.exe" nil 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   UI Settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; General

;; Theme
(require-package 'modus-themes)
(modus-themes-select 'modus-operandi-deuteranopia)

(setq modus-themes-to-toggle '(modus-operandi-deuteranopia modus-vivendi-deuteranopia))
(global-set-key (kbd "C-c h t") #'modus-themes-toggle)


;; Font
(require 'cl-seq)

(defvar dark-themes
  '("modus-vivendi-deuteranopia"
    "modus-vivendi-tinted"
    "modus-vivendi"))

(defvar light-themes
  '("modus-operandi-deuteranopia"
    "modus-operandi-tinted"
    "modus-operandi"))

(defun light-or-dark-theme ()
  (if (cl-member (car custom-enabled-themes) dark-themes :test #'string-equal)
      "dark"
    "light"))

(defun light-theme-faces ()
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
		      :weight 'semilight))

(defun dark-theme-faces ()
  (set-face-attribute 'default nil
		      :family "Iosevka Comfy"
		      :height 105
		      :weight 'medium)
  (set-face-attribute 'variable-pitch nil
		      :family "Iosevka Comfy Motion Duo"
		      :height 105
		      :weight 'medium)
  (set-face-attribute 'fixed-pitch nil
		      :family "Iosevka Comfy"
		      :height 105
		      :weight 'medium))

(defun modify-face ()
  (if (string-equal (light-or-dark-theme) "dark")
      (dark-theme-faces)
    (light-theme-faces)))

(defvar after-enable-theme-hook nil
   "Normal hook run after enabling a theme.")

(defun run-after-enable-theme-hook (&rest _args)
   "Run `after-enable-theme-hook'."
   (run-hooks 'after-enable-theme-hook))

(advice-add 'enable-theme :after #'run-after-enable-theme-hook)

(add-hook 'after-enable-theme-hook #'modify-face)

;; Modeline
(setq display-time-format " %a %e %b, %H:%M ")
(display-time-mode)
(display-battery-mode)

;; Line-numbers
(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (setq display-line-numbers-type 'relative)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'org-mode-hook 'display-line-numbers-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Evil
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-package 'evil)
(evil-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Development Settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Compilation Buffer
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; Electric Mode
(add-hook 'after-init-hook #'electric-pair-mode)
(add-hook 'after-init-hook #'electric-indent-mode)

;; Magit
(unless (package-installed-p 'magit)
  (package-install 'magit))
(require 'magit)
(global-set-key (kbd "C-x g") #'magit-status)

;; Terminal
(require-package 'eat)

;; Direnv
(require-package 'envrc)
(add-hook 'after-init-hook #'envrc-global-mode)

;; Eglot
(require-package 'eglot)

;;; Hooks
(add-hook 'python-ts-mode #'eglot-ensure)

;; ;;; Bindings
;; (global-set-key (kbd "C-c l l") #'eglot)

;; (with-eval-after-load 'eglot
;;   (global-set-key (kbd "C-c l c") #'eglot-reconnect)
;;   (global-set-key (kbd "C-c l d") #'flymake-show-buffer-diagnostics)
;;   (global-set-key (kbd "C-c l f f") #'eglot-format)
;;   (global-set-key (kbd "C-c l f b") #'eglot-format-buffer)
;;   (global-set-key (kbd "C-c l r n") #'eglot-rename)
;;   (global-set-key (kbd "C-c l s") #'eglot-shutdown)
;;   (global-set-key (kbd "C-c l i") #'eglot-inlay-hints-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Language Settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C Programming
(add-hook 'c-mode-hook 'c-ts-mode)
(add-hook 'c-ts-mode-hook (lambda ()
			    (setq-local c-ts-mode-indent-style 'linux)
			    (setq-local c-ts-mode-indent-offset 8)))

;; Python Programming
(add-hook 'python-mode-hook 'python-ts-mode)

;;; Setup Emacs-Pet
(require-package 'f)
(require 'pet)
(add-hook 'python-base-mode-hook #'pet-mode -10)

;;; Make emacs compilation buffer recognize pyright
(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist-alist
               '(pyright "^[[:blank:]]+\\(.+\\):\\([0-9]+\\):\\([0-9]+\\).*$" 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'pyright))

(with-eval-after-load 'python-ts-mode
  (defun rye-add (package)
    (interactive)
    (async-shell-command (concat "rye add " package))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Org Mode
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'org)

;; Keybindings
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c a") #'org-agenda)

(setopt org-directory "/home/slinde/org/"
	org-default-notes-file (concat org-directory "inbox.org")
	org-agenda-files `(,org-directory))

(setq org-capture-templates
      `(("t" "todo" entry (file "")  ; "" => `org-default-notes-file'
         "* NEXT %?\n%U\n" :clock-resume t)
        ("n" "note" entry (file "")
         "* %? :NOTE:\n%U\n%a\n" :clock-resume t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer/completion settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Minibuffer
(savehist-mode)  ;; Enable last used commands

;; Vertico
(require-package 'vertico)
(add-hook 'after-init-hook #'vertico-mode)

;;; Hide commands in M-x that does not work in the current mode.
(setopt read-extended-command-predicate #'command-completion-default-include-p)

;; Marginalia
(require-package 'marginalia)
(add-hook 'after-init-hook #'marginalia-mode)

;; Embark
(require-package 'embark)

;; This functions as an alternative which-key.
;; Instead of showing all the commands automatically, you hit C-h after to get
;; a list of possible actions.
(setq prefix-help-command #'embark-prefix-help-command)
(global-set-key (kbd "C-.") #'embark-act)
(global-set-key (kbd "M-.") #'embark-dwim)
(global-set-key (kbd "C-h B") #'embark-bindings)

;; Orderless
(require-package 'orderless)
(setopt completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion))))

;; Corfu
(require-package 'corfu)
(setopt corfu-auto t
	corfu-cycle t
	corfu-auto-delay 0.1
	corfu-auto-prefix 1
	corfu-popupinfo-delay '(0.5 . 0.25))
(setopt tab-always-indent 'complete)
(add-hook 'corfu-mode-hook #'corfu-popupinfo-mode)

(add-hook 'prog-mode-hook #'corfu-mode)
(add-hook 'org-mode-hook #'corfu-mode)

(define-key corfu-map (kbd "RET") nil)
(define-key corfu-map (kbd "C-y") #'corfu-insert)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Load
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'init-eww)

;;; init.el ends here

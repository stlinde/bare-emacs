;;; init.el --- -*- lexical-binding: t; coding: utf-8 -*-
;;; Commentary:
;;; Code:

;; Package Repositories
(require 'package)
(add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)

(defun require-package (package)
  (unless (package-installed-p package)
    (package-install package))
  (require package))

;; No Littering
(unless (package-installed-p 'no-littering)
  (package-install 'no-littering))
(require 'no-littering)

;; Setup PATH
(require-package 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Backups
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;; UI
;; Theme
(modus-themes-select 'modus-operandi-deuteranopia)

(setq modus-themes-to-toggle '(modus-operandi-deuteranopia modus-vivendi-deuteranopia))
(global-set-key (kbd "C-c h t") #'modus-themes-toggle)


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
  (setq display-line-numbers-type 'relative)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'org-mode-hook 'display-line-numbers-mode))

;; Minibuffer
(fido-vertical-mode t)

;; Editing

;; Electric Mode
(add-hook 'after-init-hook #'electric-pair-mode)
(add-hook 'after-init-hook #'electric-indent-mode)

;; Magit
(unless (package-installed-p 'magit)
  (package-install 'magit))
(require 'magit)
(global-set-key (kbd "C-x g") #'magit-status)

;; C Programming
(add-hook 'c-mode-hook 'c-ts-mode)
(add-hook 'c-ts-mode-hook (lambda ()
			    (setq-local c-ts-mode-indent-style 'linux)
			    (setq-local c-ts-mode-indent-offset 8)))

;; Terminal
(require-package 'eat)

;; Org-mode
(require-package 'org)

;; Keybindings
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c a") #'org-agenda)

(setq org-directory "/home/slinde/org/"
      org-default-notes-file (concat org-directory "inbox.org"))

(setq org-capture-templates
      `(("t" "todo" entry (file "")  ; "" => `org-default-notes-file'
         "* NEXT %?\n%U\n" :clock-resume t)
        ("n" "note" entry (file "")
         "* %? :NOTE:\n%U\n%a\n" :clock-resume t)))

(setq browse-url-function 'eww-browse
      shr-use-colors nil
      shr-folding-mode t)

(global-set-key (kbd "C-c w") 'eww)

(require-package 'language-detection)
(require 'cl-lib)

(defun eww-tag-pre (dom)
  (let ((shr-folding-mode 'none)
        (shr-current-font 'default))
    (shr-ensure-newline)
    (insert (eww-fontify-pre dom))
    (shr-ensure-newline)))

(defun eww-fontify-pre (dom)
  (with-temp-buffer
    (shr-generic dom)
    (let ((mode (eww-buffer-auto-detect-mode)))
      (when mode
        (eww-fontify-buffer mode)))
    (buffer-string)))

(defun eww-fontify-buffer (mode)
  (delay-mode-hooks (funcall mode))
  (font-lock-default-function mode)
  (font-lock-default-fontify-region (point-min)
                                    (point-max)
                                    nil))

(defun eww-buffer-auto-detect-mode ()
  (let* ((map '((ada ada-mode)
                (awk awk-mode)
                (c c-mode)
                (cpp c++-mode)
                (clojure clojure-mode lisp-mode)
                (csharp csharp-mode java-mode)
                (css css-mode)
                (dart dart-mode)
                (delphi delphi-mode)
                (emacslisp emacs-lisp-mode)
                (erlang erlang-mode)
                (fortran fortran-mode)
                (fsharp fsharp-mode)
                (go go-mode)
                (groovy groovy-mode)
                (haskell haskell-mode)
                (html html-mode)
                (java java-mode)
                (javascript javascript-mode)
                (json json-mode javascript-mode)
                (latex latex-mode)
                (lisp lisp-mode)
                (lua lua-mode)
                (matlab matlab-mode octave-mode)
                (objc objc-mode c-mode)
                (perl perl-mode)
                (php php-mode)
                (prolog prolog-mode)
                (python python-mode)
                (r r-mode)
                (ruby ruby-mode)
                (rust rust-mode)
                (scala scala-mode)
                (shell shell-script-mode)
                (smalltalk smalltalk-mode)
                (sql sql-mode)
                (swift swift-mode)
                (visualbasic visual-basic-mode)
                (xml sgml-mode)))
         (language (language-detection-string
                    (buffer-substring-no-properties (point-min) (point-max))))
         (modes (cdr (assoc language map)))
         (mode (cl-loop for mode in modes
                        when (fboundp mode)
                        return mode)))
    (message (format "%s" language))
    (when (fboundp mode)
      mode)))

(setq shr-external-rendering-functions
      '((pre . eww-tag-pre)))
;;; init.el ends here

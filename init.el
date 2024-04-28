;;; init.el --- -*- lexical-binding: t; coding: utf-8 -*-
;;; Commentary:
;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Config Settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vim-p
  "Use evil mode?")
(setq vim-p t)

(defvar first-setup-p
  "t setting up on new machine")
(setq first-setup-p nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Packaging Settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))


(defun install-package (package)
  (unless (package-installed-p package)
    (package-install package)))

(defun install-packages (packages)
  (dolist (package packages)
    (install-package package)))

(defun require-package (package)
  (install-package package)
  (require package))

(defun require-packages (packages)
  (dolist (package packages)
    (require-package package)))

(defun install-vc-package (package package-url)
  (unless (package-installed-p package)
    (package-vc-install package-url)))

(defun require-vc-package (package package-url)
  (install-vc-package package package-url)
  (require package))

;; Place code inside lisp and site-lisp for loading modules
(add-to-list 'load-path (concat user-emacs-directory "site-lisp/"))
(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   General Settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Turn warnings off
(setq native-comp-async-report-warnings-errors 'silent)

;; GCMH
(require-package 'gcmh)
(gcmh-mode 1)

;; No Littering
(require-package 'no-littering)

;; Setup PATH
(require-package 'exec-path-from-shell)
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "PAT")

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
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Theme
(require-packages '(modus-themes ef-themes))

;; Customize modus-themes
(setopt modus-themes-to-toggle '(modus-operandi-deuteranopia modus-vivendi-deuteranopia))
(global-set-key (kbd "C-c h t") #'modus-themes-toggle)

;; Customize ef-themes
(setopt ef-themes-to-toggle '(ef-deuteranopia-dark ef-deuteranopia-light)
	ef-themes-mixed-fonts t
	ef-themes-variable-pitch-ui t)
(mapc #'disable-theme custom-enabled-themes)


(defun shl/ef-themes-mode-line ()
  "Tweak the style of the mode lines."
  (ef-themes-with-colors
    (custom-set-faces
     `(mode-line ((,c :background ,bg-active :foreground ,fg-main :box (:line-width 1 :color ,fg-dim))))
     `(mode-line-inactive ((,c :box (:line-width 1 :color ,bg-active)))))))

(defun shl/ef-themes-hl-todo-faces ()
  "Configure `hl-todo-keyword-faces' with Ef themes colors.
     The exact color values are taken from the active Ef theme."
  (ef-themes-with-colors
    (setq hl-todo-keyword-faces
	  `(("HOLD" . ,yellow)
	    ("TODO" . ,red)
	    ("NEXT" . ,blue)
	    ("THEM" . ,magenta)
	    ("PROG" . ,cyan-warmer)
	    ("OKAY" . ,green-warmer)
	    ("DONT" . ,yellow-warmer)
	    ("FAIL" . ,red-warmer)
	    ("BUG" . ,red-warmer)
	    ("DONE" . ,green)
	    ("NOTE" . ,blue-warmer)
	    ("KLUDGE" . ,cyan)
	    ("HACK" . ,cyan)
	    ("TEMP" . ,red)
	    ("FIXME" . ,red-warmer)
	    ("XXX+" . ,red-warmer)
	    ("REVIEW" . ,red)
	    ("DEPRECATED" . ,yellow)))))

(add-hook 'ef-themes-post-load-hook #'shl/ef-themes-hl-todo-faces)
(add-hook 'ef-themes-post-load-hook #'shl/ef-themes-mode-line)

;; Font

(require 'cl-seq)

(defvar dark-themes
  '("modus-vivendi-deuteranopia"
    "modus-vivendi-tinted"
    "modus-vivendi"
    "ef-bio"
    "ef-dark"
    "ef-rosa"
    "ef-night"
    "ef-autumn"
    "ef-cherie"
    "ef-winter"
    "ef-duo-dark"
    "ef-elea-dark"
    "ef-symbiosis"
    "ef-trio-dark"
    "ef-maris-dark"
    "ef-melissa-dark"
    "ef-tritanopia-dark"
    "ef-deuteranopia-dark"
    "lambda-dark"
    "lambda-dark-faded"))

(defvar light-themes
  '("modus-operandi-deuteranopia"
    "modus-operandi-tinted"
    "modus-operandi"
    "ef-day"
    "ef-frost"
    "ef-light"
    "ef-cyprus"
    "ef-kassio"
    "ef-spring"
    "ef-summer"
    "ef-arbutus"
    "ef-duo-light"
    "ef-elea-light"
    "ef-trio-light"
    "ef-maris-light"
    "ef-melissa-light"
    "ef-tritanopia-light"
    "ef-deuteranopia-light"
    "lambda-light"
    "lambda-light-faded"))

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
		      :height 110
		      :weight 'semibold)
  (set-face-attribute 'variable-pitch nil
		      :family "Iosevka Comfy Motion Duo"
		      :height 110
		      :weight 'semibold)
  (set-face-attribute 'fixed-pitch nil
		      :family "Iosevka Comfy"
		      :height 110
		      :weight 'semibold))

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

;; Load theme
(modus-themes-select 'modus-vivendi)


;; Modeline
(setq display-time-format " %a %e %b, %H:%M ")
(display-time-mode)
(display-battery-mode)

(require-package 'doom-modeline)
(add-hook 'after-init-hook #'doom-modeline-mode)

;; Line-numbers
(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (setq display-line-numbers-type 'relative)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'org-mode-hook 'display-line-numbers-mode))

;;; Tabs
(require 'tab-bar)

(defun shl/tab-bar-tab-name-format (tab i)
    (let ((current-p (eq (car tab) 'current-tab))
    (propertize
    (concat
	" "
	(propertize " " 'display '(space :width (4)))
	(alist-get 'name tab)
	(or (and tab-bar-close-button-show
		(not (eq tab-bar-close-button-show
			(if current-p 'non-selected 'selected)))
		tab-bar-close-button)
	    "")
	(propertize " " 'display '(space :width (4))))
    'face (funcall tab-bar-tab-face-function tab)))))

;; See https://github.com/rougier/nano-modeline/issues/33
(defun shl/tab-bar-suffix ()
    "Add empty space.
This ensures that the last tab's face does not extend to the end
of the tab bar."
    " ")

(setopt tab-bar-show t
	tab-bar-tab-hints nil
	tab-bar-new-tab-choice "*scratch*"
	tab-bar-select-tab-modifiers '(super)
	tab-bar-close-tab-select 'recent
	tab-bar-new-tab-to 'rightmost
	tab-bar-tab-name-format-function #'shl/tab-bar-tab-name-format
	tab-bar-format '(tab-bar-format-history
			tab-bar-format-tabs
			shl/tab-bar-suffix
			tab-bar-format-add-tab))

;; Tabspaces
(require-package 'tabspaces)
(define-key project-prefix-map (kbd "p") #'tabspaces-open-or-create-project-and-workspace)
(add-hook 'after-init-hook #'tabspaces-mode)

(setopt tabspaces-use-filtered-buffers-as-default t
	tabspaces-default-tab "Home")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Editing
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use expand-region to select regions of text
(require-package 'expand-region)
(global-set-key (kbd "M-h") #'er/expand-region)

;; Electric Mode
(add-hook 'after-init-hook #'electric-pair-mode)
(add-hook 'after-init-hook #'electric-indent-mode)

;; Avy
(require-package 'avy)
(setopt avy-timeout-seconds 0.2)
(global-set-key (kbd "M-j") #'avy-goto-char-timer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Evil
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when vim-p
  (progn
    (install-packages '(evil evil-commentary))

    (setopt evil-want-integration t
	    evil-want-keybinding t
	    evil-want-C-u-scroll nil
	    evil-want-C-i-jump t
	    evil-respect-visual-line-mode t)

    ;; Ready to setup evil mode
    (require 'evil)
    (evil-mode 1)
    (evil-commentary-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Development Settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Compilation Buffer
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; Magit
(unless (package-installed-p 'magit)
  (package-install 'magit))
(require 'magit)
(global-set-key (kbd "C-x g") #'magit-status)

;; Diff-hl
;;; This is nice to ensure that we always know what has been changed in the
;;; current file.
(require-package 'diff-hl)
(add-hook 'after-init-hook #'global-diff-hl-mode)
(setopt diff-hl-show-staged-changes nil)
(global-set-key (kbd "C-x v c") #'magit-commit)
(global-set-key (kbd "C-x v s") #'diff-hl-stage-dwim)
(global-set-key (kbd "C-x v S") #'vc-create-tag)

;; Terminal
(require-package 'eat)

;; Direnv
(require-package 'envrc)
(add-hook 'after-init-hook #'envrc-global-mode)

;; Eglot
(require-package 'eglot)

;;; Hooks
(add-hook 'python-ts-mode #'eglot-ensure)
(add-hook 'c-mode-hook #'eglot-ensure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Language Settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C Programming
;; (add-hook 'c-mode-hook 'c-ts-mode)
;; (add-hook 'c-ts-mode-hook (lambda ()
;; 			    (setq-local c-ts-mode-indent-style 'linux)
;; 			    (setq-local c-ts-mode-indent-offset 8)))

(require-packages '(cc-mode cmake-mode))
(setopt c-default-style "linux")

;; CMake


;; Python Programming
(add-hook 'python-mode-hook 'python-ts-mode)

;;; Setup Emacs-Pet
;;; Using only envrc-mode gets our LSP working, but not the Inferior Python shell.
;;; To ensure that we use the right python shell, we need to use pet.
;;; We might be able to pull out the required functionality.
;;; Currently there is a bug in pet when running Emacs head.
;;; Thus, I've downloaded the file and changed the lines - there is already a pull request.
(require-package 'f)
(require 'pet)
(add-hook 'python-base-mode-hook #'pet-mode -10)

;;; Make emacs compilation buffer recognize pyright
(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist-alist
               '(pyright "^[[:blank:]]+\\(.+\\):\\([0-9]+\\):\\([0-9]+\\).*$" 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'pyright))


;;; Rye Functionality

;; Maybe ensure that this only shows output on errors
(defun rye-add (package)
  (interactive "sPackage: ")
  (async-shell-command (concat "rye add " package)))

(defun rye-add-dev (package)
  (interactive)
  (async-shell-command (concat "rye add --dev " package)))

(defun rye-remove (package)
  (interactive "sPackage: ")
  (async-shell-command (concat "rye remove " package)))

(defun rye-lint-this-buffer ()
  (interactive)
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (setq-default compilation-directory default-directory)
  (compilation-start (concat "rye lint "
			     (buffer-file-name (window-buffer
						(minibuffer-selected-window))))))

(defun rye-lint-project ()
  (interactive)
  (let ((default-directory (project-root (project-current t)))
        (compilation-buffer-name-function
         (or project-compilation-buffer-name-function
             compilation-buffer-name-function)))
    (save-some-buffers (not compilation-ask-about-save)
                       compilation-save-buffers-predicate)
    (setq-default compilation-directory default-directory)
    (compilation-start "rye lint")))

(defun rye-fmt-this-buffer ()
  (interactive)
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (setq-default compilation-directory default-directory)
  (compilation-start (concat "rye fmt "
			     (buffer-file-name (window-buffer
						(minibuffer-selected-window))))))

(defun rye-fmt-project ()
  (interactive)
  (let ((default-directory (project-root (project-current t)))
        (compilation-buffer-name-function
         (or project-compilation-buffer-name-function
             compilation-buffer-name-function)))
    (save-some-buffers (not compilation-ask-about-save)
                       compilation-save-buffers-predicate)
    (setq-default compilation-directory default-directory)
    (compilation-start "rye fmt")))

;;; Bind rye commands
(define-key python-ts-mode-map (kbd "C-c p R") #'rye-remove)
(define-key python-ts-mode-map (kbd "C-c p a") #'rye-add)
(define-key python-ts-mode-map (kbd "C-c p A") #'rye-add-dev)
(define-key python-ts-mode-map (kbd "C-c p l") #'rye-lint-this-buffer)
(define-key python-ts-mode-map (kbd "C-c p L") #'rye-lint-project)
(define-key python-ts-mode-map (kbd "C-c p f") #'rye-fmt-this-buffer)
(define-key python-ts-mode-map (kbd "C-c p F") #'rye-fmt-project)


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

(setopt org-directory "/home/slinde/memex/"
	org-default-notes-file (concat org-directory "inbox.org")
	org-agenda-files `(,org-directory))

(setq org-capture-templates
      `(("t" "todo" entry (file "")  ; "" => `org-default-notes-file'
         "* TODO %?\n%U\n" :clock-resume t)
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
(defun setup-corfu ()
  (require-package 'corfu)
  (setopt corfu-auto t
	  corfu-cycle t
	  corfu-auto-delay 0.1
	  corfu-auto-prefix 1
	  corfu-popupinfo-delay '(0.5 . 0.25))
  (setopt tab-always-indent 'complete)
  (setopt text-mode-ispell-word-completion nil)
  (add-hook 'corfu-mode-hook #'corfu-popupinfo-mode)


  (add-hook 'prog-mode-hook #'corfu-mode)
  (add-hook 'org-mode-hook #'corfu-mode)

  (define-key corfu-map (kbd "RET") nil)
  (define-key corfu-map (kbd "C-y") #'corfu-insert))

(defun setup-company ()
  (require-package 'company)

  (add-hook 'prog-mode-hook #'company-mode))

(setup-company)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Miscellaneous
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Spell Checking
(setq ispell-local-dictionary "en_US")
(setq ispell-program-name "hunspell")
(setq ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
(when (boundp 'ispell-hunspell-dictionary-alist)
      (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist))

(require-package 'jinx)
(add-hook 'org-mode-hook 'jinx-mode)
(setq jinx-languages "en_US")

;; PDF
(require-package 'pdf-tools)
(when first-setup-p
  (pdf-tools-install))

;; Beardbolt
(when first-setup-p
  (shell-command (concat "git clone https://github.com/joaotavora/beardbolt.git " user-emacs-directory "/beardbolt"))
  (shell-command "cd beardbolt && make"))
(add-to-list 'load-path (concat user-emacs-directory "beardbolt"))
(require 'beardbolt)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Load
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'init-eww)

;;; init.el ends here

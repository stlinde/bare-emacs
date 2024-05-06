;;; init.el --- -*- lexical-binding: t; coding: utf-8 -*-
;;; Commentary:
;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Config Settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar shl::ui::theme 'modus-operandi-deuteranopia
  "Theme to use")

(defvar vim-p
  "Use evil mode?")
(setq vim-p nil)

(defvar first-setup-p
  "t setting up on new machine")
(setq first-setup-p nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   SHL standard library
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shl::ui::set-theme (theme)
  "Set theme, and run custom load hooks."
  (cond ((member theme modus-themes-items)
	 (modus-themes-select theme))
	((member theme ef-themes-items)
	 (ef-themes-select theme))
	(load-theme theme :no-confirm)))

(defun shl::package::install (package)
  (unless (package-installed-p package)
    (package-install package)))

(defun shl::package::install-mult (packages)
  (dolist (package packages)
    (shl::package::install package)))

(defun shl::package::require (package)
  (shl::package::install package)
  (require package))

(defun shl::package::require-mult (packages)
  (dolist (package packages)
    (shl::package::require package)))

(defun shl::package::vc-install (package package-url)
  (unless (package-installed-p package)
    (package-vc-install package-url)))

(defun shl::package::vc-require (package package-url)
  (shl::package::vc-install package package-url)
  (require package))

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


;; Place code inside lisp and site-lisp for loading modules
(add-to-list 'load-path (concat user-emacs-directory "site-lisp/"))
(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   General Settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Start emacs server
;; (server-start)

;; Turn of bells
(setq ring-bell-function 'ignore)

;; Turn warnings off
(setq native-comp-async-report-warnings-errors 'silent)

;; GCMH
(shl::package::require 'gcmh)
(gcmh-mode 1)

;; No Littering
(shl::package::require 'no-littering)

;; Setup PATH
(shl::package::require 'exec-path-from-shell)
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "PAT")
(exec-path-from-shell-copy-env "NOTES")

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
(menu-bar-mode -1)

;; Theme
(shl::package::require-mult '(modus-themes ef-themes))

;; Customize modus-themes
(setopt modus-themes-to-toggle '(modus-operandi-deuteranopia modus-vivendi-deuteranopia))
(global-set-key (kbd "C-c h t") #'modus-themes-toggle)

;; Customize ef-themes
(setopt ef-themes-to-toggle '(ef-deuteranopia-dark ef-deuteranopia-light)
	ef-themes-mixed-fonts t
	ef-themes-variable-pitch-ui t)
(mapc #'disable-theme custom-enabled-themes)


(defun shl::ui::ef-themes-mode-line ()
  "Tweak the style of the mode lines."
  (ef-themes-with-colors
    (custom-set-faces
     `(mode-line ((,c :background ,bg-active :foreground ,fg-main :box (:line-width 1 :color ,fg-dim))))
     `(mode-line-inactive ((,c :box (:line-width 1 :color ,bg-active)))))))

(defun shl::ui::ef-themes-hl-todo-faces ()
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

(add-hook 'ef-themes-post-load-hook #'shl::ui::ef-themes-hl-todo-faces)
(add-hook 'ef-themes-post-load-hook #'shl::ui::ef-themes-mode-line)

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

(defun shl::ui::light-or-dark-theme ()
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

(defun shl::ui::modify-face ()
  (if (string-equal (shl::ui::light-or-dark-theme) "dark")
      (dark-theme-faces)
    (light-theme-faces)))

(defvar after-enable-theme-hook nil
   "Normal hook run after enabling a theme.")

(defun shl::ui::run-after-enable-theme-hook (&rest _args)
   "Run `after-enable-theme-hook'."
   (run-hooks 'after-enable-theme-hook))

(advice-add 'enable-theme :after #'shl::ui::run-after-enable-theme-hook)

(add-hook 'after-enable-theme-hook #'shl::ui::modify-face)

;; Load theme
(shl::ui::set-theme shl::ui::theme)


;; Modeline
(setq display-time-format " %a %e %b, %H:%M ")
(display-time-mode)
(display-battery-mode)

(shl::package::require 'doom-modeline)
(add-hook 'after-init-hook #'doom-modeline-mode)

;; Line-numbers
(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (setq display-line-numbers-type 'relative)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'org-mode-hook 'display-line-numbers-mode))

;;; Tabs
(require 'tab-bar)

(defun shl::ui::tab-bar-format-menu-bar ()
      "Produce the Menu button for the tab bar that shows the menu bar."
      `((menu-bar menu-item (propertize " 𝝺 " 'face 'tab-bar-tab-inactive)
                  tab-bar-menu-bar :help "Menu Bar")))

(defun shl::ui::tab-bar-tab-name-format (tab i)
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
(defun shl::ui::tab-bar-suffix ()
  "Add empty space.
This ensures that the last tab's face does not extend to the end
of the tab bar."
  " ")

(setopt tab-bar-show t
	tab-bar-tab-hints t
	tab-bar-new-tab-choice "*scratch*"
	tab-bar-select-tab-modifiers '(super)
	tab-bar-close-tab-select 'recent
	tab-bar-new-tab-to 'rightmost
	tab-bar-new-button nil
	tab-bar-close-button nil
	tab-bar-auto-width nil
	tab-bar-format '(shl::ui::tab-bar-format-menu-bar
			 tab-bar-format-history
			 tab-bar-format-tabs
			 shl::ui::tab-bar-suffix
			 tab-bar-format-align-right
			 tab-bar-format-global))

(add-to-list 'tab-bar-format #'tab-bar-format-menu-bar)



;; Tabspaces
(shl::package::require 'tabspaces)
(define-key project-prefix-map (kbd "p") #'tabspaces-open-or-create-project-and-workspace)
(add-hook 'after-init-hook #'tabspaces-mode)

(setopt tabspaces-use-filtered-buffers-as-default t
	tabspaces-default-tab "Home")

;;; Treemacs
(shl::package::require-mult '(treemacs
		    treemacs-tab-bar
		    treemacs-nerd-icons
		    treemacs-magit))

(setopt treemacs-follow-after-init t
	treemacs-width 35
	treemacs-indentation 2
	treemacs-collapse-dirs 3
	treemacs-is-never-other-window t
	treemacs-sorting 'alphabetic-asc
	treemacs-show-hidden-files nil)

(with-eval-after-load 'treemacs
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (treemacs-git-commit-diff-mode t))

(global-set-key (kbd "C-c e") #'treemacs)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Editing
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use expand-region to select regions of text
(shl::package::require 'expand-region)
(global-set-key (kbd "M-h") #'er/expand-region)

;; Electric Mode
(add-hook 'after-init-hook #'electric-pair-mode)
(add-hook 'after-init-hook #'electric-indent-mode)

;; Avy
(shl::package::require 'avy)
(setopt avy-timeout-seconds 0.2)
(global-set-key (kbd "M-j") #'avy-goto-char-timer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Evil
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when vim-p
  (progn
    (shl::package::install-mult '(evil evil-commentary))

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
(shl::package::require 'diff-hl)
(add-hook 'after-init-hook #'global-diff-hl-mode)
(setopt diff-hl-show-staged-changes nil)
(global-set-key (kbd "C-x v c") #'magit-commit)
(global-set-key (kbd "C-x v s") #'diff-hl-stage-dwim)
(global-set-key (kbd "C-x v S") #'vc-create-tag)

;; Terminal
(shl::package::require 'eat)

;; Direnv
(shl::package::require 'envrc)
(add-hook 'after-init-hook #'envrc-global-mode)

;; Eglot
(shl::package::require 'eglot)

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

(shl::package::require-mult '(cc-mode cmake-mode))
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
(shl::package::require 'f)
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
(shl::package::require 'org)

;; Keybindings
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c a") #'org-agenda)

(setopt org-directory (getenv "NOTES")
	org-default-notes-file (concat org-directory "work/inbox.org")
	org-agenda-files `(,org-directory))

(setq org-capture-templates
      `(("t" "work todo" entry (file+headline "work/inbox.org" "Tasks")
         "* TODO %?\n%U\n" :clock-resume t)
	("T" "personal todo" entry (file+headline "personal/inbox.org" "Notes")
         "* TODO %?\n%U\n" :clock-resume t)
        ("n" "work note" entry (file+headline "work/inbox.org" "Tasks")
         "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
	("N" "personal note" entry (file+headline "personal/inbox.org" "Notes")
	 "* %? :NOTE:\n%U\n%a\n" :clock-resume t)))

(shl::package::install 'org-modern)
(global-org-modern-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer/completion settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Minibuffer
(savehist-mode)  ;; Enable last used commands

;; Vertico
(shl::package::require 'vertico)
(add-hook 'after-init-hook #'vertico-mode)

;;; Hide commands in M-x that does not work in the current mode.
(setopt read-extended-command-predicate #'command-completion-default-include-p)

;; Marginalia
(shl::package::require 'marginalia)
(add-hook 'after-init-hook #'marginalia-mode)

;; Embark
(shl::package::require 'embark)

;; This functions as an alternative which-key.
;; Instead of showing all the commands automatically, you hit C-h after to get
;; a list of possible actions.
(setq prefix-help-command #'embark-prefix-help-command)
(global-set-key (kbd "C-.") #'embark-act)
(global-set-key (kbd "M-.") #'embark-dwim)
(global-set-key (kbd "C-h B") #'embark-bindings)

;; Orderless
(shl::package::require 'orderless)
(setopt completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion))))

;; Corfu
(defun shl::completion::setup-corfu ()
  (shl::package::require 'corfu)
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

(defun shl::completion::setup-company ()
  (shl::package::require 'company)

  (setopt company-idle-delay 0.2
	  company-dabbrev-downcase nil
	  company-minimum-prefix-length 1
	  company-selection-wrap-around t
	  company-tooltip-align-annotations t
	  company-dabbrev-ignore-case nil)
  (setopt tab-always-indent 'complete)

  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "C-y") #'company-complete-selection)
  
  (add-hook 'prog-mode-hook #'company-mode))

(shl::completion::setup-company)

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

(shl::package::require 'jinx)
(add-hook 'org-mode-hook 'jinx-mode)
(setq jinx-languages "en_US")

;; PDF
(shl::package::require 'pdf-tools)
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

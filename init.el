;; Do some basic interface changes
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(if (display-graphic-p)
  (progn (tool-bar-mode -1)
	 (scroll-bar-mode -1))
  (menu-bar-mode -1))
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<f5>") 'revert-buffer)
(defalias 'list-buffers 'ibuffer)
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; Load custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
	     '("org" . "http://orgmode.org/elpa/"))

(package-initialize)

;; Bootstrap `use-packages'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
	     :ensure t)

(use-package which-key
	     :ensure t
	     :config
	     (which-key-mode))

;; Swiper mode
(use-package counsel
  :ensure t
  )

(use-package swiper
  :ensure t
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    )
  :bind
  (("C-c C-r"	.	ivy-resume)
   ("M-x"	.	counsel-M-x)
   ("C-s"	.	swiper)
   ("C-x C-f"	.	counsel-find-file)
   ("C-c s"	.	counsel-ag)
   ))

;; Avy
(use-package avy
  :ensure t
  :bind
  ("C-'" . avy-goto-char)
  )

;; Ace window
(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind
  ("C-x o" . ace-window))

;; Auto complete
(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)
    ))

;; Themes
(use-package color-theme-sanityinc-tomorrow
  :ensure t)

(load-theme 'sanityinc-tomorrow-bright)

;; Syntax checking
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode t))

;; Magit
(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status))

;; Projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-global-mode))

;; Dired+
(use-package dired+
  :ensure t)

;; Org mode
(use-package org
  :ensure t
  )

(use-package org-journal
  :ensure t
  :init
  (setq org-journal-dir "~/Dropbox/journal"))

;; Exec path from shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;; YASnippet
(use-package yasnippet
  :ensure t
  :init (add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
  :config
  (yas-global-mode))

;; Smart mode line
(use-package smart-mode-line
  :ensure t
  :config (sml/setup))

;; Smartparens
(use-package smartparens
  :ensure t
  :config (smartparens-global-mode))

;; Languages

;; Rust
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")


;; After package loads


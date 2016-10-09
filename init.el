;; Do some basic interface changes
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(if (display-graphic-p)
  (progn (tool-bar-mode -1)
	 (scroll-bar-mode -1))
  (menu-bar-mode -1))
(fset 'yes-or-no-p 'y-or-n-p)
(defalias 'list-buffers 'ibuffer)
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; Recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 30)

;; Load custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")
	("org" . "http://orgmode.org/elpa/")
	("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;; Bootstrap `use-packages'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Use-package config
(setq use-package-always-ensure t)

(use-package try)

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

;; Swiper mode
(use-package counsel)

(use-package swiper
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    )
  :diminish ivy-mode
  :bind
  (("C-c C-r"	.	ivy-resume)
   ("M-x"	.	counsel-M-x)
   ("C-s"	.	swiper)
   ("C-x C-f"	.	counsel-find-file)
   ("C-c s"	.	counsel-ag)
   ("C-x C-r"   .       ivy-recentf)
   ))

;; Avy
(use-package avy
  :bind
  ("C-'" . avy-goto-char)
  )

;; Ace window
(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind
  ("C-x o" . ace-window))

;; Auto complete
;; (use-package auto-complete
;;   :init
;;   (progn
;;     (ac-config-default)
;;     (global-auto-complete-mode t)
;;     )
;;   :diminish auto-complete-mode)

(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  :diminish 'company-mode
  )

;; Diminish
(use-package diminish)
(diminish 'auto-revert-mode)

;; Themes
(use-package color-theme-sanityinc-tomorrow)
(use-package gruvbox-theme)

(load-theme 'gruvbox)

;; Syntax checking
(use-package flycheck
  :config
  (global-flycheck-mode t)
  :diminish flycheck-mode)

;; Magit
(use-package magit
  :bind
  ("C-x g" . magit-status))
}
;; Projectile
(use-package projectile
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-global-mode))

;; Dired+
(use-package dired+)

;; Org mode
(use-package org)

(use-package org-journal
  :init
  (setq org-journal-dir "~/Dropbox/journal"))

;; Exec path from shell
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;; YASnippet
(use-package yasnippet
  :init (add-hook 'term-mode-hook (lambda ()
				    (yas-minor-mode -1)))
  :load-path "~/.emacs.d/plugins/yasnippet"
  :diminish yas-minor-mode
  :config
  (yas-global-mode)
  :bind (("C-c i s" . yas-insert-snippet)
	 ("M-/" . yas-expand)))

;; Multi-term
(use-package multi-term
  :init (setq multi-term-program "/bin/zsh")
  :bind
  (("C-c C-'" . multi-term-dedicated-toggle)
   ("C-c '" . multi-term)
   :map term-mode-map
   ("M-]" . multi-term-next)
   ("M-[" . multi-term-prev)
   ))

;; Spaceline
;; (use-package spaceline-config
;;   :ensure spaceline
;;   :config (spaceline-emacs-theme)
;;   )

;; Telephone line
(use-package telephone-line
  :config (telephone-line-mode 1))

;; Smartparens
(use-package smartparens
  :config (smartparens-global-mode)
  :bind
  (("M-[" . sp-forward-barf-sexp)
   ("M-]" . sp-forward-slurp-sexp))
  :diminish smartparens-mode)

;; Eyebrowse
(use-package eyebrowse
  :config (eyebrowse-mode t))

;; Fancy battery
(use-package fancy-battery
  :init (add-hook 'after-init-hook #'fancy-battery-mode))

;; Languages

;; C/C++
(use-package irony
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)

  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; Python
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode))

(use-package ein)

;; Rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (when (fboundp 'sp-local-pair)
    (sp-local-pair 'rust-mode "'" nil :actions nil)))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package cargo
  :config (add-hook 'rust-mode-hook 'cargo-minor-mode))

;; Golang
(use-package go-mode
  :mode ("\\.go\\'" . go-mode))

;; Elm
(use-package elm-mode
  :mode "\\.elm\\'"
  :config (add-hook 'elm-mode-hook #'elm-oracle-setup-ac))

;; Scala
(use-package ensime
  :pin melpa-stable)

;; Clojure
(use-package clojure-mode
  :mode "\\.clj\\'"
  )

(use-package cider)

;; Haskell

(use-package haskell-mode
  :mode ("(\\.hs\\'|\\.lhs')" . haskell-mode)
  :interpreter ("stack ghci" . haskell-mode))


;; After package loads

;; Colorize compilation buffers
(setq-default compilation-scroll-output t)
(ignore-errors
  (require 'ansi-color)
  (defun hallfox/colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'hallfox/colorize-compilation-buffer))

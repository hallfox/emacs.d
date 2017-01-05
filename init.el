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
(global-hl-line-mode)
(global-auto-revert-mode)
(pending-delete-mode t)

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

;; My defuns
(defun indent-buffer ()
  "Indent current buffer according to major mode."
  (interactive)
  (indent-region (point-min) (point-max)))

(global-set-key (kbd "C-c TAB") 'indent-buffer)

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
   ("C-x C-r"   .       counsel-recentf)
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

;; Projectile

(defun projectile-open-multiterm-at-root ()
  "Invoke multi-term at project's root."
  (interactive)
  (projectile-with-default-dir (projectile-project-root) (multi-term)))

(use-package projectile
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-global-mode)
  :bind (:map projectile-mode-map
	      ("C-c p $" . projectile-open-multiterm-at-root))
  )

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
  :bind (:map yas-minor-mode-map
	      ("C-c i s" . yas-insert-snippet)
	      ("M-/" . yas-expand)))

;; Multi-term
(use-package multi-term
  :init (setq multi-term-program "/bin/zsh")
  :bind
  (("C-c T" . multi-term-dedicated-toggle)
   ("C-c t" . multi-term)
   :map term-mode-map
   ("M-]" . multi-term-next)
   ("M-[" . multi-term-prev)
   ))

;; Telephone line
(use-package telephone-line
  :config (telephone-line-mode 1))

;; Google
(use-package google-this
  :diminish google-this-mode
  :config (google-this-mode 1))

;; Smartparens
(use-package smartparens
  :init (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
  :config (require 'smartparens-config)
  :bind (:map smartparens-mode-map
	      ("C-M-f" . sp-forward-sexp)
	      ("C-M-b" . sp-backward-sexp)
	      ("C-M-n" . sp-next-sexp)
	      ("C-M-p" . sp-previous-sexp)
	      ("C-M-a" . sp-begginning-of-sexp)
	      ("C-M-e" . sp-end-of-sexp)
	      ("C-M-d" . sp-kill-sexp)
	      ("C-M-DEL" . sp-backward-kill-sexp)
	      ("C-M-t" . sp-transpose-sexp)
	      ("C-(" . sp-backward-slurp-sexp)
	      ("C-)" . sp-forward-slurp-sexp)
	      ("C-S-(" . sp-backward-barf-sexp)
	      ("C-S-)" . sp-forward-barf-sexp)
	      ("C-c (" . sp-rewrap-sexp)
	      ("C-c )" . sp-unwrap-sexp)
	      ))

;; Expand region
(use-package expand-region
  :config (require 'expand-region)
  :bind ("C-=" . er/expand-region))

;; Eyebrowse
(use-package eyebrowse
  :config (eyebrowse-mode t))

;; Fancy battery
(use-package fancy-battery
  :init (add-hook 'after-init-hook #'fancy-battery-mode))

;; Languages

;; C/C++

;; Python

(setq py-python-command "python3")
(setq python-shell-interpreter "python3")

(use-package company-jedi
  :init (add-hook 'python-mode-hook (lambda ()
				      (add-to-list 'company-backends 'company-jedi))))

(use-package elpy
  :init (setq elpy-rpc-python-command "python3")
  :config (elpy-enable))

;; Rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :bind (:map rust-mode-map ("C-c TAB" . rust-format-buffer))
  :config
  (when (fboundp 'sp-local-pair)
    (sp-local-pair 'rust-mode "'" nil :actions nil)))

(use-package flycheck-rust
  :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package cargo
  :init (add-hook 'rust-mode-hook 'cargo-minor-mode))

;; Golang
(use-package go-mode
  :mode ("\\.go\\'" . go-mode))

;; Elm
(use-package elm-mode
  :mode "\\.elm\\'"
  :init (add-hook 'elm-mode-hook #'elm-oracle-setup-ac))

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
  :config (setq haskell-process-type 'stack-ghci)
  :bind (:map haskell-mode-map
	      ("C-c C-z" . haskell-interactive-bring)
	      ("C-c C-l" . haskell-process-load-or-reload)
	      ("C-c C-t" . haskell-process-do-type)
	      ("C-c C-k" . haskell-interactive-mode-clear)))

;; Elixir
(use-package elixir-mode
  :mode "(\\.ex\\'|\\.exs')")

(use-package alchemist)


;; After package loads

;; Colorize compilation buffers
(setq-default compilation-scroll-output t)
(ignore-errors
  (require 'ansi-color)
  (defun hallfox/colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'hallfox/colorize-compilation-buffer))

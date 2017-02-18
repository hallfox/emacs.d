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
  (winner-mode t))
(global-hl-line-mode t)
(global-auto-revert-mode t)
(pending-delete-mode t)
(setq tramp-default-method "ssh")

;; Prettify symbols
(defconst lisp--prettify-symbols-alist
  '(("lambda" . ?λ)))
(add-hook 'emacs-lisp-mode 'prettify-symbols-mode)
(add-hook 'scheme-mode 'prettify-symbols-mode)

;; Recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 30)

;; Compile shortcuts
(global-set-key (kbd "C-x m") 'compile)
(global-set-key (kbd "C-x C-m") 'recompile)

;; Load custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Load secrets file
(setq secrets-file (expand-file-name "secrets.el" user-emacs-directory))
(when (file-exists-p secrets-file)
  (load secrets-file))

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")
	("org" . "http://orgmode.org/elpa/")
	("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(package-refresh-contents)

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

;; ivy
(use-package ivy
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
  ("C-j" . avy-goto-char)
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
;(use-package color-theme-sanityinc-tomorrow)
;(use-package gruvbox-theme)
(use-package ample-theme
  :init (progn (load-theme 'ample t t)
               (load-theme 'ample-flat t t)
               (load-theme 'ample-light t t)
               (enable-theme 'ample))
  :defer t)

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
  (projectile-mode)
  :bind (:map projectile-mode-map
	      ("C-c p $" . projectile-open-multiterm-at-root))
  )

;; Dired+
(use-package dired+)

;; Org mode
(use-package org
  :bind ("C-c c" . org-capture)
  :config
  (progn
    (add-hook 'org-mode-hook #'(lambda () (auto-fill-mode t)))

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (elixir . t)))
    ))

(setq org-agenda-files (list "~/Dropbox/org/gcal.org"
			     "~/Dropbox/org/i.org"))

(setq org-capture-templates
      '(("a" "Appointment" entry (file  "~/Dropbox/org/gcal.org" "Appointments")
	 "* TODO %?\n:PROPERTIES:\n\n:END:\nDEADLINE: %^T \n %i\n")
	("n" "Note" entry (file+headline "~/Dropbox/org/notes.org" "Notes")
	 "* Note %?\n%T")
	("l" "Link" entry (file+headline "~/Dropbox/org/links.org" "Links")
	 "* %? %^L %^g \n%T" :prepend t)
	("t" "To Do Item" entry (file+headline "~/Dropbox/org/i.org" "To Do Items")
	 "* %?\n%T" :prepend t)
	("j" "Journal" entry (file+datetree "~/Dropbox/journal.org")
	 "* %?\nEntered on %U\n  %i\n  %a")
	))

(use-package org-gcal
  :init (progn
	  (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync)))
	  (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync)))))

(use-package org-journal
  :init
  (setq org-journal-dir "~/Dropbox/org/journal")
  (unbind-key "C-'" org-mode-map)
  )

(use-package ox-reveal)

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

;; ;; Multi-term
(setenv "PAGER" "cat")

(use-package multi-term
  :init (setq multi-term-program "/bin/zsh")
  :bind
  (("C-c t" . multi-term-dedicated-toggle)
   ("C-'" . multi-term)
   :map term-mode-map
   ("M-]" . multi-term-next)
   ("M-[" . multi-term-prev)
   ))

;; Telephone line
(use-package telephone-line
  :config (telephone-line-mode 1))

;; Smartparens
(use-package smartparens
  :config
  (progn
    (require 'smartparens-config)
    (sp-local-pair 'c-mode "{" nil :post-handlers '((my-create-newline-and-enter "RET")))
    (sp-local-pair 'c++-mode "{" nil :post-handlers '((my-create-newline-and-enter "RET"))))
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

(defun my-create-newline-and-enter (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))


;; Expand region
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Visual Rexp
(use-package visual-regexp
  :bind ("M-%" . vr/query-replace))
(use-package visual-regexp-steroids)

;; zzz-to-char
(use-package zzz-to-char
  :bind (("M-z" . zzz-to-char)
	 ("C-c M-z" . zzz-up-to-char)))

(use-package easy-kill
  :config (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package aggressive-indent)

;; Neotree
(use-package neotree
  :config (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; persp-mode
(use-package persp-mode
  :init
  (setq persp-autokill-buffer-on-remove 'kill-weak)
  (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))

(with-eval-after-load "persp-mode"
  (with-eval-after-load "ivy"
    (add-hook 'ivy-ignore-buffers
              #'(lambda (b)
                  (when persp-mode
                    (let ((persp (get-current-persp)))
                      (if persp
                          (not (persp-contain-buffer-p b persp))
                        nil)))))

    (setq ivy-sort-functions-alist
          (append ivy-sort-functions-alist
                  '((persp-kill-buffer   . nil)
                    (persp-remove-buffer . nil)
                    (persp-add-buffer    . nil)
                    (persp-switch        . nil)
                    (persp-window-switch . nil)
                    (persp-frame-switch  . nil))))))

;; Fancy battery
(use-package fancy-battery
  :init (add-hook 'after-init-hook #'fancy-battery-mode))

;; Nice icons
(use-package all-the-icons)

;; Neotree
(use-package neotree
  :config (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  )

;; Hydras
(use-package hydra)

(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("=" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("0" (text-scale-adjust 0) "rest")
  ("q" nil "quit" :color blue))

;; Apps
(use-package ledger-mode
  :mode "\\.ledger$")

(use-package google-this)

;; Languages

;; C/C++
(setq c-default-style "gnu"
      c-basic-offset 2)

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

(use-package toml-mode)

;; Golang
(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :config
  (add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1))))

;; Elm
(use-package elm-mode
  :mode "\\.elm\\'"
  :init (add-hook 'elm-mode-hook #'elm-oracle-setup-ac))

;; Scala
(use-package ensime
  :pin melpa-stable)

;; Clojure
(use-package clojure-mode
  :mode "\\.clj\\'")

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

(use-package intero)

;; Elixir
(use-package elixir-mode
  :mode "(\\.ex\\'|\\.exs')")

(use-package alchemist)
(use-package ob-elixir)

;; OCaml
(use-package tuareg)


;; After package loads

;; Colorize compilation buffers
(setq-default compilation-scroll-output t)
(ignore-errors
  (require 'ansi-color)
  (defun hallfox/colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'hallfox/colorize-compilation-buffer))

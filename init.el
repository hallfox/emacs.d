;; Do some basic interface changes
(fset 'yes-or-no-p 'y-or-n-p)
(when (fboundp 'winner-mode)
  (winner-mode t))
(setq inhibit-startup-screen t)
(global-hl-line-mode t)
(global-auto-revert-mode t)
(pending-delete-mode t)
(setq tramp-default-method "ssh")
(show-paren-mode t)
(setq show-paren-style 'expression)

;; Prettify symbols
(defconst lisp--prettify-symbols-alist
  '(("lambda"           . ?λ)))
(add-hook 'emacs-lisp-mode 'prettify-symbols-mode)
(add-hook 'scheme-mode 'prettify-symbols-mode)

;; Recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 30)

;; Mac keyboard fix
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))

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
      '(("melpa"        . "https://melpa.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")
	("org"          . "http://orgmode.org/elpa/")
	("gnu"          . "https://elpa.gnu.org/packages/")))

(package-initialize)
(package-refresh-contents)

;; Bootstrap quelpa
(if (require 'quelpa nil t)
    (quelpa-self-upgrade)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap . el")
    (eval-buffer)))

;; Bootstrap `quelpa-use-package'
(quelpa
 '(quelpa-use-package
   :fetcher github
   :repo "quelpa/quelpa-use-package"))
(require 'quelpa-use-package)

;; Very important
(setq use-package-always-ensure t)

;; My defuns
(defun indent-buffer ()
  "Indent current buffer according to major mode . "
  (interactive)
  (indent-region (point-min) (point-max)))

(global-set-key (kbd "C-c TAB") 'indent-buffer)

(use-package try)
(use-package better-defaults
  :config (setq visible-bell nil))

(use-package which-key
  :defer t
  :diminish which-key-mode
  :config
  (which-key-mode))

;; ivy, swiper, counsel
(use-package ivy)
(use-package swiper)
(use-package counsel
  :init
  (setq ivy-use-virtual-buffers t
	;; Format for completions
	ivy-count-format "(%d/%d) "
	ivy-wrap t
	;; Get rid of . / and ../
	ivy-extra-directories nil)
  :config
  (ivy-mode 1)
  :diminish ivy-mode
  :bind
  (("C-c C-r" . ivy-resume)
   ("M-x"     . counsel-M-x)
   ("C-s"     . swiper)
   ("C-x C-f" . counsel-find-file)
   ("C-c s"   . counsel-rg)
   ("C-x C-r" . counsel-recentf)
   ("C-c j"   . counsel-imenu)
   ("C-x r b" . counsel-bookmark)
   ("M-y"     . counsel-yank-pop)
   ("C-c m"   .	counsel-mark-ring)
   ("C-h f"   .	counsel-describe-function)
   ("C-h v"   .	counsel-describe-variable)
   ("C-c l"   .	counsel-locate)))

(use-package helm-make
  :init
  (setq helm-make-completion-method 'ivy)
  :bind
  ("C-x C-M-m" . helm-make)
  :ensure helm)

(use-package avy
  :bind
  (("C-;" . avy-goto-char)
   ("C-x C-SPC" . avy-pop-mark)))

;; (use-package helm
;;   :defer t
;;   :init
;;   (unbind-key "C-x c")
;;   (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
;;       helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
;;       helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
;;       helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
;;       helm-ff-file-name-history-use-recentf t
;;       helm-echo-input-in-header-line t)
;;   :config
;;   (progn
;;     (require 'helm-config)
;;     (use-package helm-swoop)

;;     (when (executable-find "curl")
;;       (setq helm-google-suggest-use-curl-p t))

;;     (defun spacemacs//helm-hide-minibuffer-maybe ()
;;   "Hide minibuffer in Helm session if we use the header line as input field."
;;   (when (with-helm-buffer helm-echo-input-in-header-line)
;;     (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
;;       (overlay-put ov 'window (selected-window))
;;       (overlay-put ov 'face
;;                    (let ((bg-color (face-background 'default nil)))
;;                      `(:background ,bg-color :foreground ,bg-color)))
;;       (setq-local cursor-type nil))))


;;     (add-hook 'helm-minibuffer-set-up-hook
;; 	      'spacemacs//helm-hide-minibuffer-maybe))
  
;;   :bind
;;   (("C-c h" . helm-command-prefix)
;;    ("C-x C-f" . helm-find-files)
;;    ("M-x" . helm-M-x)
;;    ("C-x r b" . helm-filtered-bookmarks)
;;    ("C-x b" . helm-buffers-list)
;;    ("C-s" . helm-swoop)
;;    ("M-y" . helm-show-kill-ring)
;;    :map helm-map
;;    ("<tab>" . helm-execute-persistent-action)
;;    ("C-i" . helm-execute-persistent-action)
;;    ("C-z" . helm-select-action))
;;   )

;; Ace window
(use-package ace-window
  :defer t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind
  (("C-x o" . ace-window)
   ("M-p" . ace-window)))

(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  :diminish 'company-mode
  )

;; Diminish
(use-package diminish)
(diminish 'auto-revert-mode)

;; mwim
(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
	 ("C-e" . mwim-end-of-code-or-line)))

;; Themes
(use-package color-theme-sanityinc-tomorrow)
;(use-package gruvbox-theme)
(use-package ample-theme
  :init (progn (load-theme 'ample t t)
               (load-theme 'ample-flat t t)
               (load-theme 'ample-light t t)
               (enable-theme 'ample-flat))
  :defer t)

;; Syntax checking
(use-package flycheck
  :config
  (global-flycheck-mode t)
  :diminish flycheck-mode)

;; Magit
(use-package magit
  :init
  (setq magit-completing-read-function 'ivy-completing-read)
  :defer t
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
  :quelpa
  )

;; Dired+
(use-package dired+
  :defer t)

;; Org mode
(use-package org
  :defer t
  :bind ("C-c c" . org-capture)
  :config
  (progn
    (add-hook 'org-mode-hook #'(lambda () (auto-fill-mode t)))

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (elixir . t)))
    ))

(setq org-agenda-files (list "~/Dropbox/org/i.org"))

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

(use-package org-journal
  :defer t
  :init
  (setq org-journal-dir "~/Dropbox/org/journal")
  )

(use-package ox-reveal)

;; Exec path from shell
(use-package exec-path-from-shell
  :defer t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;; YASnippet
(use-package yasnippet
  :defer t
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

;; (use-package multi-term
;;   :defer t
;;   :init (setq multi-term-program "/usr/local/bin/fish")
;;   :bind
;;   (("C-c t" . multi-term-dedicated-toggle)
;;    ("C-'" . multi-term)
;;    :map term-mode-map
;;    ("M-]" . multi-term-next)
;;    ("M-[" . multi-term-prev)
;;    ))

;; eshell
(global-set-key (kbd "C-c t") 'eshell)
(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell-cmpl-initialize)
            (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
            (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)))
(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map
              (kbd "M-p")
              'helm-eshell-history)))

(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*eshell*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (eshell))
    (switch-to-buffer-other-window "*eshell*")))
(global-set-key (kbd "C-c '") 'visit-term-buffer)

;; Telephone line
;; (use-package telephone-line
;;   :config (telephone-line-mode 1))

;; Smartparens
(use-package smartparens
  :config
  (progn
    (require 'smartparens-config)
    (sp-local-pair 'c-mode "{" nil :post-handlers '((my-create-newline-and-enter "RET")))
    (sp-local-pair 'c++-mode "{" nil :post-handlers '((my-create-newline-and-enter "RET")))
    (smartparens-mode t))
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
  :defer t
  :bind ("C-=" . er/expand-region))

(use-package aggressive-indent)

;; Neotree
(use-package neotree
  :defer t
  :config (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; persp-mode
(use-package persp-mode
  :defer t
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
  :defer t
  :init (add-hook 'after-init-hook #'fancy-battery-mode))

;; Nice icons
(use-package all-the-icons
  :defer t)

;; Hydras
(use-package hydra
  :defer t)

(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("=" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("0" (text-scale-adjust 0) "rest")
  ("q" nil "quit" :color blue))

;; Apps
(use-package ledger-mode
  :defer t
  :mode "\\.ledger$")

;; Languages

;; C/C++
(setq c-default-style "gnu"
      c-basic-offset 2)

;; Python
(setq py-python-command "python3")
(setq python-shell-interpreter "python3")

(use-package company-jedi
  :defer t
  :init (add-hook 'python-mode-hook (lambda ()
				      (add-to-list 'company-backends 'company-jedi))))

(use-package elpy
  :defer t
  :init (setq elpy-rpc-python-command "python3")
  :config (elpy-enable))

;; Rust
(use-package rust-mode
  :defer t
  :mode "\\.rs\\'"
  :bind (:map rust-mode-map ("C-c TAB" . rust-format-buffer))
  :config
  (when (fboundp 'sp-local-pair)
    (sp-local-pair 'rust-mode "'" nil :actions nil)))

(use-package flycheck-rust
  :defer t
  :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package cargo
  :defer t
  :init (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package toml-mode
  :defer t)

;; Golang
(use-package go-mode
  :defer t
  :mode ("\\.go\\'" . go-mode)
  :config
  (add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1))))

;; Elm
(use-package elm-mode
  :defer t
  :mode "\\.elm\\'"
  :init (add-hook 'elm-mode-hook #'elm-oracle-setup-ac))

;; Scala
(use-package ensime
  :defer t)

;; Clojure
(use-package clojure-mode
  :defer t
  :mode "\\.clj\\'")

(use-package cider
  :defer t)

;; Haskell
(use-package haskell-mode
  :defer t
  :mode ("(\\.hs\\'|\\.lhs')" . haskell-mode)
  :config (setq haskell-process-type 'stack-ghci)
  :bind (:map haskell-mode-map
	      ("C-c C-z" . haskell-interactive-bring)
	      ("C-c C-l" . haskell-process-load-or-reload)
	      ("C-c C-t" . haskell-process-do-type)
	      ("C-c C-k" . haskell-interactive-mode-clear)))

(use-package intero
  :defer t)

;; Elixir
(use-package elixir-mode
  :defer t
  :mode "(\\.ex\\'|\\.exs')")

(use-package alchemist
  :defer t)
(use-package ob-elixir
  :defer t)

;; OCaml
(use-package tuareg
  :defer t)

;; Racket
(use-package racket-mode
  :defer t)

;; After package loads

;; Colorize compilation buffers
(setq-default compilation-scroll-output t)
(ignore-errors
  (require 'ansi-color)
  (defun hallfox/colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'hallfox/colorize-compilation-buffer))

(setq debug-on-error t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;; General emacs configuration
(use-package emacs
  :init   ; executed before package is loaded
  (fset 'yes-or-no-p 'y-or-n-p)
  (put 'narrow-to-region 'disabled nil)
  :hook   ; add functions onto hook; only basename needed
  ((before-save . whitespace-cleanup)
   (text-mode   . turn-on-auto-fill))
  :bind   ; key bindings
  (("C-x a r" . align-regexp))
  :custom ; options set by customize-variable
  (inhibit-startup-screen t)
  (default-input-method "TeX")
  :config ; executed after package is loaded
  (defadvice  ; align-regexp should use spaces.
      align-regexp
      (around align-regexp-with-spaces activate)
    (let ((indent-tabs-mode nil)) ad-do-it))
  :custom-face
  (default ((t (:family "Input Mono" :foundry "FBI " :slant normal :weight normal :height 89 :width normal)))))

(use-package auto-complete
  :ensure t
  :config
  (ac-config-default)
  (ac-set-trigger-key "TAB")
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict")
  :hook
  ((Coq-mode latex-mode sml-mode) . auto-complete-mode))

(use-package auto-indent-mode
  :ensure t
  :config
  (auto-indent-global-mode)
  :custom
  (auto-indent-next-pair-timer-interval
   (quote ((css-mode 1.5)
	   (haskell-mode 1.5)
	   (python-mode 1.5)
	   (latex-mode 1.5)
	   (coq-mode 1.5)
	   (default 0.0005)))))

(use-package cc-mode
  :custom
  (c-default-style "bsd"))

(use-package cus-edit
  :custom
  (custom-file null-device "Don't store customizations"))

(use-package dictem
  :load-path "~/.emacs.d/dictem/")

(use-package dtrt-indent
  :ensure t)

(use-package elpy
  :ensure t
  :after python
  :config (elpy-enable)
  :custom
  (elpy-rpc-python-command "python3"))

(use-package epa-file ; EasyPG, bunled with emacs for encrypted files
  :config
  (epa-file-enable))

(use-package ess
  :ensure t)

(use-package fill-column-indicator
  :ensure t)

(use-package haskell-mode
  :ensure t
  :hook ((haskell-mode . turn-on-haskell-doc-mode)
         (haskell-mode . turn-on-haskell-indentation)))

(use-package ispell
  :ensure t
  :defer t
  :custom
  (ispell-dictionary "en_CA-w_accents"))

(use-package magit
  :ensure t
  :config
  (put 'magit-clean 'disabled nil))

(use-package markdown-mode
  :ensure t
  :init
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.mdown\'" . markdown-mode)))

(use-package menu-bar
  :config
  (menu-bar-mode -1)
  :bind
  ([S-f10] . menu-bar-mode))

(use-package merlin
  :ensure t)

(use-package minibuffer
  :config
  (add-to-list 'completion-styles 'substring)
  :custom
  ;; Cycle  through tab-completions with tab if there are less than 5
  (setq completion-cycle-threshold 5))

(use-package moe-theme
  :ensure t
  :after powerline ; must be loaded after for powerline-moe-theme to work
  :config
  (moe-dark)
  (moe-theme-set-color 'blue)
  (powerline-moe-theme))

(use-package ocp-indent
  :ensure t)

(use-package octave
  :mode ("\\.m$" . octave-mode))

(use-package outline
  :ensure t)

(use-package outline-magic
  :ensure t
  :hook ((outline-mode . (lambda () (require 'outline-cycle))))
  :bind (:map outline-minor-mode-map
	      ([f10] . outline-cycle)))

(use-package paren
  :config
  (show-paren-mode t)
  :custom
  ;; highlight entire expression when on delimiters
  (show-paren-style 'expression))

(use-package proof-general
  :ensure t
  :custom
  (proof-disappearing-proofs t)
  (proof-electric-terminator-enable t)
  (proof-three-window-enable t)
  (proof-three-window-mode-policy 'hybrid)
  :custom-face
  (proof-locked-face ((t (:weight bold)))))

(use-package py-autopep8
  :ensure t
  :hook (python-mode . py-autopep8-enable-on-save)
  :custom
  (py-autopep8-options '("--max-line-length=80")))

(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :custom (python-python-command "/usr/bin/python3")
  :interpreter ("python3" . python-mode)
  :custom
  (python-indent-guess-indent-offset nil)
  (python-indent-offset 4)
  (python-shell-interpreter "python3"))

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)
	 (latex-mode . rainbow-delimiters-mode)))

(use-package rainbow-mode
  :ensure t)

(use-package reftex
  :ensure t
  :custom
  (reftex-label-alist '(("axiom" ?a "ax:" "~\\ref{%s}" nil ("axiom" "ax.") -3)
                        ("theorem" ?h "theorem:" "~\\ref{%s}" t ("theorem" "th.") -3)
                        ("proposition" ?p "prop:" "~\\ref{%s}" t ("proposition" "prop.") -3)
                        ("definition" ?d "def:" "~\\ref{%s}" t ("definition" "def.") -3)
                        ("corollary" ?c "cor:" "~\\ref{%s}" t ("corollary" "cor.") -3)
                        ("lemma" ?l "lemma:" "~\\ref{%s}" t ("lemma" "lem.") -3)
                        ("conjecture" ?j "conj:" "~\\ref{%s}" t ("conjecture" "conj.") -3)
                        ("ax" ?a "ax:" "~\\ref{%s}" nil ("axiom" "ax.") -3)
                        ("thm" ?h "theorem:" "~\\ref{%s}" t ("theorem" "th.") -3)
                        ("prop" ?p "prop:" "~\\ref{%s}" t ("proposition" "prop.") -3)
                        ("defi" ?d "def:" "~\\ref{%s}" t ("definition" "def.") -3)
                        ("cor" ?c "cor:" "~\\ref{%s}" t ("corollary" "cor.") -3)
                        ("lem" ?l "lemma:" "~\\ref{%s}" t ("lemma" "lem.") -3)
                        ("conj" ?j "conj:" "~\\ref{%s}" t ("conjecture" "conj.") -3))))

(use-package sass-mode
  :ensure t)

(use-package scroll-bar
  :custom
  (scroll-bar-mode nil))

(use-package sendmail
  :custom
  (mail-specify-envelope-from t)
  (mail-envelope-from 'header)
  (message-send-mail-function 'sendmail-send-it))

(use-package simple
  :custom
  (column-mode-number t))

(use-package spaceline
  :ensure t
  :requires spaceline-config)

(use-package sml-mode
  :ensure t)

(use-package tex                        ; TeX editing/processing
  :ensure auctex
  :defer t
  :init
  ;; http://www.cs.au.dk/~abizjak/emacs/2016/03/06/latex-fill-paragraph.html
  (defun ales/fill-paragraph (&optional P)
    "When called with prefix argument call `fill-paragraph'.
Otherwise split the current paragraph into one sentence per line."
    (interactive "P")
    (if (not P)
        (save-excursion
          (let ((fill-column 12345678)) ;; relies on dynamic binding
            (fill-paragraph) ;; this will not work correctly if the paragraph is
            ;; longer than 12345678 characters (in which case the
            ;; file must be at least 12MB long. This is unlikely.)
            (let ((end (save-excursion
                         (forward-paragraph 1)
                         (backward-sentence)
                         (point-marker))))  ;; remember where to stop
              (beginning-of-line)
              (while (progn (forward-sentence)
                            (<= (point) (marker-position end)))
                (just-one-space) ;; leaves only one space, point is after it
                (delete-char -1) ;; delete the space
                (newline)        ;; and insert a newline
                (LaTeX-indent-line) ;; I only use this in combination with late, so this makes sense
                ))))
      ;; otherwise do ordinary fill paragraph
      (fill-paragraph P)))
  :hook
  ((LaTeX-mode . turn-on-reftex)
   (LaTeX-mode . turn-off-auto-fill)
   (LaTeX-mode . turn-on-flyspell)
   (LaTeX-mode . outline-minor-mode)
   (LaTeX-mode . LaTeX-math-mode)
   (LaTeX-mode . rainbow-delimiters-mode)
   (LaTeX-mode . (lambda ()
                   (LaTeX-add-environments
                    '("axiom" LaTeX-env-label)
                    '("theorem" LaTeX-env-label)
                    '("proposition" LaTeX-env-label)
                    '("definition" LaTeX-env-label)
                    '("corollary" LaTeX-env-label)
                    '("lemma" LaTeX-env-label)
                    '("conjecture" LaTeX-env-label)))))
  :config
  (add-to-list 'TeX-view-program-selection '(output-pdf "Zathura"))
  (add-to-list 'LaTeX-label-alist '("axiom" . "ax:"))
  (add-to-list 'LaTeX-label-alist '("conjecture" . "conj:"))
  (add-to-list 'LaTeX-label-alist '("corollary" . "cor:"))
  (add-to-list 'LaTeX-label-alist '("definition" . "def:"))
  (add-to-list 'LaTeX-label-alist '("proposition" . "prop:"))
  (add-to-list 'LaTeX-label-alist '("theorem" . "theorem:"))
  (add-to-list 'LaTeX-label-alist '("lemma" . "conj:"))
  :custom
  (TeX-parse-self t) ; Parse documents to provide completion for packages, etc.
  (TeX-auto-save t) ; Automatically save style information
  (TeX-clean-confirm nil) ; Don't ask for confirmation when cleaning
  (TeX-source-correlate-mode t) ; Provide forward and inverse search with SyncTeX
  (TeX-source-correlate-method 'synctex)
  (reftex-plug-into-AUCTeX t) ; use reftex to get ToC: C-c = to explore structure of TeX doc
  (TeX-PDF-mode t) ; use pdftex by default
  :bind
  (:map LaTeX-mode-map
        ("M-q" . ales/fill-paragraph)))

(use-package tex-site
  :ensure auctex)

(use-package tool-bar
  :config
  (tool-bar-mode -1))

(use-package tramp
  :defer t
  :custom
  (tramp-default-method "ssh"))

(use-package tuareg
  :ensure t
  :config
  (add-hook 'tuareg-mode-hook 'merlin-mode t))

(use-package vc-git
  :requires vc
  :config
  (add-to-list 'vc-handled-backends 'git))

(use-package windmove
  :config
  (windmove-default-keybindings)
  :custom
  (windmove-wrap-around t))

(use-package yaml-mode
  :ensure t)

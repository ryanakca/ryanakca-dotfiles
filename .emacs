(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;; LaTeX with AUCTeX
(use-package tex-site                   ; AUCTeX initialization
  :ensure auctex)

(use-package tex                        ; TeX editing/processing
  :ensure auctex
  :defer t
  :config
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook
	    (lambda ()
	      (LaTeX-add-environments
	       '("axiom" LaTeX-env-label)
	       '("theorem" LaTeX-env-label)
	       '("proposition" LaTeX-env-label)
	       '("definition" LaTeX-env-label)
	       '("corollary" LaTeX-env-label)
	       '("lemma" LaTeX-env-label)
	       '("conjecture" LaTeX-env-label))))
  (add-to-list 'TeX-view-program-list
	       '("Zathura"
		 ("zathura "
		  (mode-io-correlate " --synctex-forward %n:0:%b -x \"emacsclient +%{line} %{input}\" ")
		  " %o")
		 "zathura"))
  (add-to-list 'TeX-view-program-selection
			    '(output-pdf "Zathura"))
  (progn
    (setq TeX-parse-self t              ; Parse documents to provide completion
					; for packages, etc.
	  TeX-auto-save t               ; Automatically save style information
	  ;; Don't ask for confirmation when cleaning
	  TeX-clean-confirm nil
	  ;; Provide forward and inverse search with SyncTeX
	  TeX-source-correlate-mode t
	  TeX-source-correlate-method 'synctex
	  ;; use reftex to get ToC: C-c = to explore structure of TeX doc
	  reftex-plug-into-AUCTeX t
	  reftex-label-alist '(("axiom"   ?a "ax:"  "~\\ref{%s}" nil ("axiom"   "ax.") -3)
			       ("theorem" ?h "theorem:" "~\\ref{%s}" t   ("theorem" "th.") -3)
			       ("proposition" ?p "prop:" "~\\ref{%s}" t   ("proposition" "prop.") -3)
			       ("definition" ?d "def:" "~\\ref{%s}" t   ("definition" "def.") -3)
			       ("corollary" ?c "cor:" "~\\ref{%s}" t   ("corollary" "cor.") -3)
			       ("lemma" ?l "lemma:" "~\\ref{%s}" t   ("lemma" "lem.") -3)
			       ("conjecture" ?j "conj:" "~\\ref{%s}" t   ("conjecture" "conj.") -3)
                               ("ax"   ?a "ax:"  "~\\ref{%s}" nil ("axiom"   "ax.") -3)
			       ("thm" ?h "theorem:" "~\\ref{%s}" t   ("theorem" "th.") -3)
			       ("prop" ?p "prop:" "~\\ref{%s}" t   ("proposition" "prop.") -3)
			       ("defi" ?d "def:" "~\\ref{%s}" t   ("definition" "def.") -3)
			       ("cor" ?c "cor:" "~\\ref{%s}" t   ("corollary" "cor.") -3)
			       ("lem" ?l "lemma:" "~\\ref{%s}" t   ("lemma" "lem.") -3)
			       ("conj" ?j "conj:" "~\\ref{%s}" t   ("conjecture" "conj.") -3)

			       )
	  )))

(use-package auto-complete
  :ensure t
  :config
  (ac-config-default)
  (ac-set-trigger-key "TAB")
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict")
  (add-to-list 'ac-modes 'Coq-mode)
  (add-to-list 'ac-modes 'latex-mode)
  (add-to-list 'ac-modes 'sml-mode)
  )

(use-package auto-indent-mode
  :ensure t)
(use-package dictem
  :load-path "~/.emacs.d/dictem/")
(use-package dtrt-indent
  :ensure t)
(use-package fill-column-indicator
  :ensure t)
(use-package haskell-mode
  :ensure t)
(use-package markdown-mode
  :ensure t)
(use-package rainbow-mode
  :ensure t)
(use-package sml-mode
  :ensure t)
(use-package yaml-mode
  :ensure t)
(use-package sass-mode
  :ensure t)
(use-package ess
  :ensure t)
(use-package merlin
   :ensure t)
(use-package ocp-indent
   :ensure t)
(use-package tuareg
  :ensure t
  :config
  (add-hook 'tuareg-mode-hook 'merlin-mode t))
(use-package py-autopep8
  :ensure t
  :config
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
  (setq py-autopep8-options '("--max-line-length=80")))
(use-package elpy
  :ensure t
  :config
  (setq elpy-rpc-python-command "python3"))


(add-hook 'before-save-hook 'whitespace-cleanup)

;;;; AUTO FILL
					; We want auto-fill enabled for all text modes
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;;; AUTO INDENT
(auto-indent-global-mode)

;;;; CC-mode, bundled with emacs
(setq c-default-style "bsd")

;;; EasyPG, bunled with emacs for encrypted files
(require 'epa-file)
(epa-file-enable)

;;;; HASKELL-MODE
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;;; MARKDOWN-MODE
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))

;;;; OCTAVE, bundled with emacs
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;;;; PROOFGENERAL
(setq proof-three-window-enable t)
(setq proof-three-window-mode-policy 'hybrid)

;;;; TeX
(setq TeX-PDF-mode t)

;;; THEME
(load-theme 'ryanakca t)

;;;; TRAMP, bundled with emacs for editing remote files
(require 'tramp)

;;;; VC-GIT
(when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))

;;;; Sending mail
(setq mail-specify-envelope-from t)
(setq mail-envelope-from 'header)
(setq message-send-mail-function 'sendmail-send-it)

;;;; Python
(setq python-python-command "/usr/bin/python3")
(elpy-enable)

;;;; Default dictionary
(setq ispell-dictionary "british-ise-w_accents")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-indent-next-pair-timer-interval (quote ((css-mode 1.5) (haskell-mode 1.5) (python-mode 1.5) (latex-mode 1.5) (coq-mode 1.5) (default 0.0005))))
 '(safe-local-variable-values (quote ((eval setq debian-changelog-mailing-address "rak@debian.org") (eval add-to-list (quote debian-changelog-allowed-distributions) "daylog"))))
 '(column-number-mode t)
 '(default-input-method "TeX")
 '(inhibit-startup-screen t)
 '(proof-disappearing-proofs t)
 '(proof-electric-terminator-enable t)
 '(python-indent-guess-indent-offset nil)
 '(python-indent-offset 4)
 '(python-shell-interpreter "python3")
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata" :foundry "unknown" :slant normal :weight normal :height 83 :width normal))))
 '(proof-locked-face ((t (:weight bold))) t))

;;; Others
;; Emacs24: Cycle through tab-completions with tab if there are less than 5
(setq completion-cycle-threshold 5)
;; Emacs24: Also match for substrings in tab completion
(add-to-list 'completion-styles 'substring)
;; align text according to a delimiter
(global-set-key (kbd "C-x a r") 'align-regexp)

;; info location:
(eval-after-load 'info
  '(add-to-list 'Info-directory-list "~/.emacs.d/share/info"))

;; Replace yes or no prompts with y or n.
(fset 'yes-or-no-p 'y-or-n-p)

(put 'narrow-to-region 'disabled nil)

(defun my-fill-latex-paragraph ()
  "Fill the current paragraph, separating sentences w/ a newline.

AUCTeX's latex.el reimplements the fill functions and is *very*
convoluted. We use part of it --- skip comment par we are in."
  (interactive)
  (if (save-excursion
	(beginning-of-line) (looking-at TeX-comment-start-regexp))
      (TeX-comment-forward)
    (let ((to (progn
		(LaTeX-forward-paragraph)
		(point)))
	  (from (progn
		  (LaTeX-backward-paragraph)
		  (point)))
	  (to-marker (make-marker)))
      (set-marker to-marker to)
      (while (< from (marker-position to-marker))
	(forward-sentence)
	(setq tmp-end (point))
	(LaTeX-fill-region-as-paragraph from tmp-end)
	(setq from (point))
	(unless (bolp)
	  (LaTeX-newline))))))


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

(eval-after-load "latex"
  '(define-key LaTeX-mode-map (kbd "M-q") 'ales/fill-paragraph))

(windmove-default-keybindings)
(setq windmove-wrap-around t)

					; align-regexp should use spaces.
(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

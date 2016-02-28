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
  (progn
    (setq TeX-parse-self t              ; Parse documents to provide completion
                                        ; for packages, etc.
          TeX-auto-save t               ; Automatically save style information
          ;; Don't ask for confirmation when cleaning
          TeX-clean-confirm nil
          ;; Provide forward and inverse search with SyncTeX
          TeX-source-correlate-mode t
          TeX-source-correlate-method 'synctex)))

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

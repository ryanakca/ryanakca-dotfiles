(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/auto-indent-mode")
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(add-to-list 'load-path "~/.emacs.d/dictem")
(add-to-list 'load-path "~/.emacs.d/popup")
(add-to-list 'load-path "~/.emacs.d/magit")
(add-to-list 'load-path "~/.emacs.d/markdown-mode")
(add-to-list 'load-path "~/.emacs.d/proofgeneral/")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/vm")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;;;; AUCTEX
(setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))
(setq TeX-view-program-selection '((output-pdf "Evince")))

;;;; AUTO COMPLETE
(require 'auto-complete-config)
(ac-config-default)
(ac-set-trigger-key "TAB")
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict")
(add-to-list 'ac-modes 'Coq-mode)
(add-to-list 'ac-modes 'latex-mode)

;;;; AUTO FILL
; We want auto-fill enabled for all text modes
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;;; AUTO INDENT
(require 'auto-indent-mode)
(auto-indent-global-mode)

;;;; DICTEM
(require 'dictem)

;;; EasyPG, bunled with emacs for encrypted files
(require 'epa-file)
(epa-file-enable)

;;;; MAGIT
(require 'magit)

;;;; MARKDOWN-MODE
(autoload 'markdown-mode "markdown-mode"
          "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

;;;; PROOFGENERAL
(load-file "~/.emacs.d/proofgeneral/generic/proof-site.el")

;;;; TeX
(setq TeX-PDF-mode t)

;;; THEME
(load-theme 'ryanakca t)

;;;; TRAMP, bundled with emacs for editing remote files
(require 'tramp)

;;;; VC-GIT
(setq load-path (cons (expand-file-name "/usr/share/doc/git-core/contrib/emacs") load-path))
(require 'vc-git)
(when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))
(require 'git)
(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git." t)

;;;; VM
(require 'vm-autoloads)
; Make 'M-x compose-mail' (C-x m), create a VM-style composition buffer
(setq mail-user-agent 'vm-user-agent)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-indent-next-pair-timer-interval (quote ((css-mode 1.5) (haskell-mode 1.5) (python-mode 1.5) (latex-mode 1.5) (coq-mode 1.5) (default 0.0005))))
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

(setq mew-proto "%")
(setq mew-imap-user "ryan")
(setq mew-imap-server "localhost")

;;; Others
;; Emacs24: Cycle through tab-completions with tab if there are less than 5
(setq completion-cycle-threshold 5)
;; Emacs24: Also match for substrings in tab completion
(add-to-list 'completion-styles 'substring)

;; info location:
(eval-after-load 'info
                 '(add-to-list 'Info-directory-list "~/.emacs.d/share/info"))

;; Replace yes or no prompts with y or n.
(fset 'yes-or-no-p 'y-or-n-p)

(put 'narrow-to-region 'disabled nil)

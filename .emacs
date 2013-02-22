(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/auto-indent-mode")
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(add-to-list 'load-path "~/.emacs.d/popup")
(add-to-list 'load-path "~/.emacs.d/magit")
(add-to-list 'load-path "~/.emacs.d/markdown-mode")
(add-to-list 'load-path "~/.emacs.d/proofgeneral/")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/vm")

(require 'epa-file)
(epa-file-enable)

;;;; AUTO COMPLETE
(require 'auto-complete-config)
(ac-config-default)
(ac-set-trigger-key "TAB")
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict")
(add-to-list 'ac-modes 'Coq-mode)

;;;; AUTO INDENT
(require 'auto-indent-mode)
(auto-indent-global-mode)

;;;; MAGIT
(require 'magit)

;;;; MARKDOWN-MODE
(autoload 'markdown-mode "markdown-mode"
          "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

;;;; PROOFGENERAL
(load-file "~/.emacs.d/proofgeneral/generic/proof-site.el")

;;;; VC-GIT
(setq load-path (cons (expand-file-name "/usr/share/doc/git-core/contrib/emacs") load-path))
(require 'vc-git)
(when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))
(require 'git)
(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git." t)

;;;; VM
(require 'vm-autoloads)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-indent-next-pair-timer-interval (quote ((css-mode 1.5) (haskell-mode 1.5) (python-mode 1.5) (latex-mode 1.5) (coq-mode 1.5) (default 0.0005))))
 '(column-number-mode t)
 '(default-input-method "TeX")
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
 '(proof-locked-face ((t (:weight bold)))))

;;;; TeX
(setq TeX-PDF-mode t)

;;;; Others
;(require 'color-theme)
;(color-theme-initialize)

(setq mew-proto "%")
(setq mew-imap-user "ryan")
(setq mew-imap-server "localhost")

;; Emacs24: Cycle through tab-completions with tab if there are less than 5
(setq completion-cycle-threshold 5)
;; Emacs24: Also match for substrings in tab completion
(add-to-list 'completion-styles 'substring)

;; info location:
(eval-after-load 'info
                 '(add-to-list 'Info-directory-list "~/.emacs.d/share/info"))

;; Replace yes or no prompts with y or n.
(fset 'yes-or-no-p 'y-or-n-p)

(set-background-color "black")
(set-face-background 'default "black")
(set-face-background 'region "black")
(set-face-foreground 'default "white")
(set-face-foreground 'region "gray60")
(set-foreground-color "white")
(set-cursor-color "red")

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/auto-indent-mode")
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(add-to-list 'load-path "~/.emacs.d/bbdb")
(add-to-list 'load-path "~/.emacs.d/dictem")
(add-to-list 'load-path "~/.emacs.d/dtrt-indent")
(add-to-list 'load-path "~/.emacs.d/haskell-mode")
(add-to-list 'load-path "~/.emacs.d/popup")
(add-to-list 'load-path "~/.emacs.d/magit")
(add-to-list 'load-path "~/.emacs.d/markdown-mode")
(add-to-list 'load-path "~/.emacs.d/proofgeneral/")
(add-to-list 'load-path "~/.emacs.d/share/emacs/site-lisp/vm")
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

;;;; CC-mode, bundled with emacs
(setq c-default-style "bsd")

;;;; DICTEM
(require 'dictem)

;;;; DTRT-INDENT
(require 'dtrt-indent)

;;; EasyPG, bunled with emacs for encrypted files
(require 'epa-file)
(epa-file-enable)

;;;; HASKELL-MODE
(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;;; MAGIT
(require 'magit)

;;;; MARKDOWN-MODE
(autoload 'markdown-mode "markdown-mode"
          "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))

;;;; OCTAVE, bundled with emacs
(autoload 'octave-mode "octave-mod" nil t)
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;;;; PROOFGENERAL
(load-file "~/.emacs.d/proofgeneral/generic/proof-site.el")
(setq proof-three-window-enable t)
(setq proof-three-window-mode-policy 'hybrid)

;;;; TeX
(setq TeX-PDF-mode t)

;;; THEME
(load-theme 'ryanakca t)

;;;; TRAMP, bundled with emacs for editing remote files
(require 'tramp)

;;;; VC-GIT
(require 'vc-git)
(when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))

;;;; VM
(require 'vm-autoloads)
; Make 'M-x compose-mail' (C-x m), create a VM-style composition buffer
(setq mail-user-agent 'vm-user-agent)

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

;; Sentence-fill hack
;; ==================
;;
;; The macros here are based on emacs/23.3/lisp/textmodes/fill.el.gz.
;; To use them without modifying emacs, you can simply execute `cat
;; hack.el >> ~/.emacs` if you have downloaded this file (say, by
;; git).  Otherwise, you can use
;;
;; curl http://fermi.mycloudnas.com/cgit.cgi/fill/plain/hack.el >> ~/.emacs

(defun auto-fill-by-sentences ()
  (if (looking-back (sentence-end))
      ;; Break at a sentence
      (progn
        (LaTeX-newline)
        t)
    ;; Fall back to the default
    (do-auto-fill)))
(add-hook 'LaTeX-mode-hook (lambda () (setq auto-fill-function 'auto-fill-by-sentences)))

;; Modified from http://pleasefindattached.blogspot.com/2011/12/emacsauctex-sentence-fill-greatly.html
(defadvice LaTeX-fill-region-as-paragraph (around LaTeX-sentence-filling)
  "Start each sentence on a new line."
  (let ((from (ad-get-arg 0))
        (to-marker (set-marker (make-marker) (ad-get-arg 1)))
        tmp-end)
    (while (< from (marker-position to-marker))
      (forward-sentence)
      ;; might have gone beyond to-marker---use whichever is smaller:
      (ad-set-arg 1 (setq tmp-end (min (point) (marker-position to-marker))))
      ad-do-it
      (ad-set-arg 0 (setq from (point)))
      (unless (or (looking-back "^\\s *")
                  (looking-at "\\s *$"))
        (LaTeX-newline)))
    (set-marker to-marker nil)))
(ad-activate 'LaTeX-fill-region-as-paragraph)

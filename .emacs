;; (setq debug-on-error t)

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
  ((text-mode   . turn-on-auto-fill)
   (before-save . whitespace-cleanup))
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

;; (use-package auctex
;;   :config
;;   ;; From
;;   ;; https://github.com/jwiegley/dot-emacs/commit/6dffc2511c60e1e15c79080bf37a36f380d9567b
;;   (defun latex-help-get-cmd-alist ()    ;corrected version:
;;     "Scoop up the commands in the index of the latex info manual.
;;    The values are saved in `latex-help-cmd-alist' for speed."
;;     ;; mm, does it contain any cached entries
;;     (if (not (assoc "\\begin" latex-help-cmd-alist))
;;	(save-window-excursion
;;	  (setq latex-help-cmd-alist nil)
;;	  (Info-goto-node (concat latex-help-file "Command Index"))
;;	  (goto-char (point-max))
;;	  (while (re-search-backward "^\\* \\(.+\\): *\\(.+\\)\\." nil t)
;;	    (let ((key (buffer-substring (match-beginning 1) (match-end 1)))
;;		  (value (buffer-substring (match-beginning 2)
;;					   (match-end 2))))
;;	      (add-to-list 'latex-help-cmd-alist (cons key value))))))
;;     latex-help-cmd-alist))

(use-package ace-window
  :ensure t
  :custom
  ;; home row keys
  (aw-keys '(?h ?t ?n ?s ?a ?o ?e ?u ?i ?d))
  :bind (("M-o" . ace-window))
  :config
  ;; need to redefine a few of these because the defaults
  ;; overlap with customized aw-keys
  (setq aw-dispatch-alist
    '((?x aw-delete-window "Delete Window")
      (?m aw-swap-window "Swap Windows")
      (?M aw-move-window "Move Window")
      (?c aw-copy-window "Copy Window")
      (?j aw-switch-buffer-in-window "Select Buffer")
      (?f aw-flip-window)
      (?p aw-switch-buffer-other-window "Switch Buffer Other Window")
      (?c aw-split-window-fair "Split Fair Window")
      (?v aw-split-window-vert "Split Vert Window")
      (?b aw-split-window-horz "Split Horz Window")
      (?1 delete-other-windows "Delete Other Windows")
      (?? aw-show-dispatch-help))))

(use-package auto-complete
  :ensure t
  :config
  (ac-config-default)
  (ac-set-trigger-key "TAB")
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict")
  :hook
  ((Coq-mode LaTeX-mode sml-mode) . auto-complete-mode))

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

(use-package bibtex
  :mode ("\\.bib\'" . bibtex-mode)
  :custom
  (bibtex-dialect 'biblatex)
  (bibtex-maintain-sorted-entries 'crossref)
  ;; Use only the first author name if there are more than 2 authors
  (bibtex-autokey-names 1)
  ;; Otherwise use both names
  (bibtex-autokey-names-stretch 1)
  (bibtex-autokey-name-separator "_")
  (bibtex-autokey-name-year-separator "_")
  (bibtex-autokey-year-length 4)
  (bibtex-autokey-year-title-separator ":_")
  (bibtex-autokey-titleword-length 5)
  (bibtex-autokey-titlewords 3)
  ;; Make all title words lowercase
  (bibtex-autokey-preserve-case 1)
  :config
  ;; Until https://debbugs.gnu.org/cgi/bugreport.cgi?bug=36252 gets fixed
  (defun bibtex-autokey-get-year ()
    "Return year field contents as a string obeying `bibtex-autokey-year-length'.
If the year field is absent, extract the year from a valid ISO8601-2
Extended Format date in the date field and return it as a string obeing
`bibtex-autokey-year-length'."
    (let ((yearfield (bibtex-autokey-get-field "year"))
	  (datefield (bibtex-autokey-get-field "date"))
	  (shortener (lambda (year)
		       (substring year (max 0 (- (length year)
						 bibtex-autokey-year-length))))))
      (if (string= "" yearfield)
	  (cond ((string-match "[./]*\\(-?[[:digit:]]+X*\\)\\([-/.[:digit:]:T~?%X]*\\)"
			       datefield)
		 ;; Matches ISO8601-2 Extended Format specification level 1
		 ;; examples listed in tables 3, 4, and 5 on pp. 38-40 of the
		 ;; biblatex package manual, version 3.12
		 (funcall shortener (match-string 1 datefield)))
		(t (error "Date field `%s' is incorrectly formed" datefield)))
	(funcall shortener yearfield))))
  ;; Don't have accented characters in keys
  (let ((charMap '(;; This list based on Xah Lee's http://ergoemacs.org/emacs/emacs_zap_gremlins.html
		   ("ß" . "ss")
		   ("á\\|à\\|â\\|ä\\|ā\\|ǎ\\|ã\\|å\\|ą\\|ă\\|ạ\\|ả\\|ả\\|ấ\\|ầ\\|ẩ\\|ẫ\\|ậ\\|ắ\\|ằ\\|ẳ\\|ặ" . "a")
		   ("æ" . "ae")
		   ("ç\\|č\\|ć" . "c")
		   ("é\\|è\\|ê\\|ë\\|ē\\|ě\\|ę\\|ẹ\\|ẻ\\|ẽ\\|ế\\|ề\\|ể\\|ễ\\|ệ" . "e")
		   ("í\\|ì\\|î\\|ï\\|ī\\|ǐ\\|ỉ\\|ị" . "i")
		   ("ñ\\|ň\\|ń" . "n")
		   ("ó\\|ò\\|ô\\|ö\\|õ\\|ǒ\\|ø\\|ō\\|ồ\\|ơ\\|ọ\\|ỏ\\|ố\\|ổ\\|ỗ\\|ộ\\|ớ\\|ờ\\|ở\\|ợ" . "o")
		   ("ú\\|ù\\|û\\|ü\\|ū\\|ũ\\|ư\\|ụ\\|ủ\\|ứ\\|ừ\\|ử\\|ữ\\|ự" . "u")
		   ("ý\\|ÿ\\|ỳ\\|ỷ\\|ỹ" . "y")
		   ("þ" . "th")
		   ("ď\\|ð\\|đ" . "d")
		   ("ĩ" . "i")
		   ("ľ\\|ĺ\\|ł" . "l")
		   ("ř\\|ŕ" . "r")
		   ("š\\|ś" . "s")
		   ("ť" . "t")
		   ("ž\\|ź\\|ż" . "z")
		   ("œ" . "oe")
		   (" " . " ") ; thin space etc
		   ("–" . "-")
		   ("—\\|一" . "--"))))
    ;; For some reason, *-name-* and *-titleword-* get clobbered, even
    ;; though the original value in decribe-variable clearly shows
    ;; them having been based on the extended
    ;; bibtex-autokey-transcriptions. Force them to be the right
    ;; thing.
    (progn (seq-do (lambda (pair) (add-to-list 'bibtex-autokey-transcriptions pair)) charMap)
	   (seq-do (lambda (pair) (add-to-list 'bibtex-autokey-name-change-strings pair)) charMap)
	   (seq-do (lambda (pair) (add-to-list 'bibtex-autokey-titleword-change-strings pair)) charMap))))

(use-package bibtex-completion
  :ensure helm-bibtex
  :custom
  (bibtex-completion-bibliography "~/Documents/papers/library.bib")
  (bibtex-completion-library-path "~/Documents/papers/pdfs/")
  (bibtex-completion-notes-path   "~/Documents/papers/notes/")
  (bibtex-completion-notes-extension ".org")
  :bind (("C-c b" . helm-bibtex)))

(use-package cc-mode
  :custom
  (c-default-style "bsd"))

(use-package cus-edit
  :custom
  (custom-file null-device "Don't store customizations"))

(use-package diary
  :custom
  (diary-display-function 'diary-fancy-display)
  :hook ((diary-list-entries . diary-include-other-diary-files)
	 (diary-list-entries . diary-sort-entries)))

(use-package dictem
  :load-path "~/.emacs.d/dictem/")

(use-package doi-utils
  ;; provided by org-ref
  :ensure org-ref)

(use-package dtrt-indent
  :ensure t)

(use-package elpher
  :ensure t)

(use-package elpy
  :ensure t
  :after python
  :config (elpy-enable)
  :custom
  (elpy-rpc-python-command "python3"))

(use-package ess
  :ensure t)

(use-package fill-column-indicator
  :ensure t)

(use-package gnus
  :custom
  (gnus-select-method
   '(nntp "news.club.cc.cmu.edu"
	  (nntp-address "news.club.cc.cmu.edu")
	  (nntp-via-address "linux.gp.cs.cmu.edu")
	  (nntp-via-rlogin-command "ssh")
	  (nntp-via-rlogin-command-switches ("-C"))
	  (nntp-open-connection-function nntp-open-via-rlogin-and-netcat)))
  (gnus-home-score-file "gnus.SCORE")
  (gnus-secondary-select-methods
   '((nntp "news.gmane.org")
     (nntp "news.eternal-september.org")
     (nntp "nntp.olduse.net")))
  (message-citation-line-function 'message-insert-formatted-citation-line)
  (gnus-update-message-archive-method t)
  (gnus-message-archive-method
	'(nnfolder "archive" ; this gets included in the server buffer
	       (nnfolder-inhibit-expiry t)
	       (nnfolder-get-new-mail nil)
	       (nnfolder-active-file "~/News/sent/active")
	       (nnfolder-directory   "~/News/sent")))
  (gnus-check-new-newsgroups nil)
  :hook
  (message-send . gnus-score-followup-article))

(use-package haskell-mode
  :ensure t
  :hook ((haskell-mode . turn-on-haskell-doc-mode)
	 (haskell-mode . turn-on-haskell-indentation)))

(use-package helm
  :ensure t
  :bind (("M-x"     . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x C-b" . helm-mini))
  :config
  (helm-mode 1))

(use-package info-look
  :ensure t)

(use-package ispell
  :ensure t
  :defer t
  :custom
  (ispell-dictionary "en_CA-w_accents"))

(use-package latex
  :ensure auctex
  :after info-look
  :mode ("\\.tex\\'" . TeX-latex-mode)
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
   (LaTeX-mode . LaTeX-math-mode)
   (LaTeX-mode . (lambda ()
		   (LaTeX-add-environments
		    '("axiom" LaTeX-env-label)
		    '("theorem" LaTeX-env-label)
		    '("proposition" LaTeX-env-label)
		    '("definition" LaTeX-env-label)
		    '("corollary" LaTeX-env-label)
		    '("lemma" LaTeX-env-label)
		    '("conjecture" LaTeX-env-label))))
   (LaTeX-mode . (lambda ()
		   ;; This must be a hook. Trying to set it in :config
		   ;; causes the variable to become buffer-local, so
		   ;; it never actually takes effect in all
		   ;; latex-moded buffers. So much time wasted debugging.
		   (add-to-list 'LaTeX-label-alist '("axiom" . "ax:"))
		   (add-to-list 'LaTeX-label-alist '("conjecture" . "conj:"))
		   (add-to-list 'LaTeX-label-alist '("corollary" . "cor:"))
		   (add-to-list 'LaTeX-label-alist '("definition" . "def:"))
		   (add-to-list 'LaTeX-label-alist '("proposition" . "prop:"))
		   (add-to-list 'LaTeX-label-alist '("theorem" . "theorem:"))
		   (add-to-list 'LaTeX-label-alist '("lemma" . "lemma:")))))

  :config
  (info-lookup-add-help :mode 'LaTeX-mode
			:regexp ".*"
			:parse-rule "\\\\?[a-zA-Z]+\\|\\\\[^a-zA-Z]"
			:doc-spec '(("(latex2e)Concept Index")
				    ("(latex2e)Command Index")))

  :bind
  (:map LaTeX-mode-map
	("M-q" . ales/fill-paragraph)))

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
  ;; ocaml assistant
  :ensure t)

(use-package mingus
  ;; mpd client
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
  :mode ("\\.m\'" . octave-mode))

(use-package org
  :ensure t
  :bind
  (("C-c l" . 'org-store-link)
   ("C-c a" . 'org-agenda)
   ("C-c c" . 'org-capture))
  :custom
  (org-agenda-files '("~/org/"))
  (org-default-notes-file "~/org/notes.org")
  (org-agenda-include-diary t)
  ;; Define the custum capture templates
  ;; Courtesy of Gregory J. Stein,
  ;; http://cachestocaches.com/2016/9/my-workflow-org-agenda/
  (org-capture-templates
   '(("t" "todo" entry (file "~/org/todo.org")
      "* TODO %?\n%u\n%a\n" :clock-in t :clock-resume t)
     ("m" "Meeting" entry (file org-default-notes-file)
      "* MEETING with %? :MEETING:\n%t" :clock-in t :clock-resume t)
     ("d" "Diary" entry (file+olp+datetree "~/org/diary.org")
      "* %?\n%U\n" :clock-in t :clock-resume t)
     ("i" "Idea" entry (file org-default-notes-file)
      "* %? :IDEA: \n%t" :clock-in t :clock-resume t)
     ("n" "Next Task" entry (file+headline org-default-notes-file "Tasks")
      "** NEXT %? \nDEADLINE: %t")))
  ;; Include org-agenda-files in the list of refile targets
  (org-refile-targets (quote ((nil :maxlevel . 9)
			      (org-agenda-files :maxlevel . 9))))
  (org-columns-default-format "%25ITEM %TODO %3PRIORITY %10CLOCKSUM %16TIMESTAMP_IA %TAGS")
  ;; Tags with fast selection keys
  (org-tag-alist (quote ((:startgroup)
			 ("@errand" . ?e)
			 ("@cmu" . ?c)
			 ("@home" . ?h)
			 (:endgroup)
			 ("WAITING" . ?W)
			 ("HOLD" . ?H)
			 ("WORK" . ?w)
			 ("PERSONAL" . ?p)
			 ("CANCELLED" . ?c)
			 ("NOTE" . ?n)
			 ("IDEA" . ?i)
			 ("FLAGGED" . ??))))
  ;;;; TODO states and shortcuts
  (org-todo-keywords
   (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	   ;; c   : shortcut for org-use-fast-todo-selection
	   ;; @   : add a note (with time)
	   ;; !   : record only the time of the state change
	   ;; x/y : use x when entering & y when leaving iff target does not define x
	   (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
  (org-todo-keyword-faces
   (quote (("TODO" :foreground "red" :weight bold)
	   ("NEXT" :foreground "blue" :weight bold)
	   ("DONE" :foreground "forest green" :weight bold)
	   ("WAITING" :foreground "orange" :weight bold)
	   ("HOLD" :foreground "magenta" :weight bold)
	   ("CANCELLED" :foreground "forest green" :weight bold)
	   ("MEETING" :foreground "forest green" :weight bold)
	   ("PHONE" :foreground "forest green" :weight bold))))
  ;; add/remove the following tags
  (setq org-todo-state-tags-triggers
	(quote (("CANCELLED" ("CANCELLED" . t))
		("WAITING" ("WAITING" . t))
		("HOLD" ("WAITING") ("HOLD" . t))
		(done ("WAITING") ("HOLD"))
		("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
		("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
		("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))
  (org-use-fast-todo-selection t)
  ;; Don't update timestamps/notes when going S-left/S-right
  (org-treat-S-cursor-todo-selection-as-state-change nil)
  ;;;; customize the agenda view
  ;; Compact view
  (org-agenda-compact-blocks t)
  ;; Custom ordering
  (org-agenda-custom-commands
   ;; each entry is of the form (key desc type match settings files)
   ;; this is based off of http://doc.norang.ca/org-mode.html#CustomAgendaViews
   ;; and should be finished at some point.
   (quote ((" " "Agenda"
	    ((agenda "")
	     (tags-todo "REFILE"
			((org-agenda-overriding-header "Tasks to Refile")
			 (org-tags-match-list-sublevels nil)))
	     (tags-todo "-CANCELLED/!NEXT"
			((org-agenda-overriding-header "Next Tasks")
			 (org-agenda-sorting-strategy
			  '(todo-state-down effort-up category-keep))))
	     (tags-todo "-CANCELLED+WAITING|HOLD/!"
			((org-agenda-overriding-header "Waiting & Postponed Tasks")))
	     (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
			((org-agenda-overriding-header "Standalone tasks")
			 (org-agenda-sorting-strategy '(category-keep))))
	     (tags "-REFILE/"
		   ((org-agenda-overriding-header "Tasks to Archive")))))
	   ("e" "Everything" ((agenda "") (alltodo ""))))))
  :config
  (add-to-list 'org-modules 'org-habit))

(use-package org-noter
  :ensure t
  :custom
  ;; Surely there's an easier way of setting this?
  (org-noter-notes-search-path '("~/Documents/papers/notes/")))

(use-package org-ref
  :requires helm-bibtex
  :custom
  (org-ref-default-bibliography '("~/Documents/papers/library.bib"))
  (org-ref-bibliography-notes "~/Documents/papers/notes.org")
  (org-ref-pdf-directory "~/Documents/papers/pdfs/")
  ;; don't fudge with the output of bibtex-generate-autokey
  (org-ref-clean-bibtex-key-function 'identity)
  ;; let helm-bibtex find the notes file for an entry
  (org-ref-notes-function
   (lambda (thekey)
     (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
       (bibtex-completion-edit-notes
	(list (car (org-ref-get-bibtex-key-and-file thekey))))))))

(use-package org-ref-arxiv
  :ensure org-ref)

(use-package org-ref-bibtex
  :ensure org-ref
  :config
  (defun org-ref-bibtex-format-url-if-doi ()
    "Override built-in function. Originally, this reformatted
the url to point to DOI. I would rather kill the URL field
entirely if it contains the DOI."
    (interactive)
    (unless (eq (org-ref-bibtex-entry-doi) "")
      (when (string-match-p (regexp-quote (org-ref-bibtex-entry-doi))
			  (bibtex-autokey-get-field "url"))
	(bibtex-kill-field "url")))))

(use-package org-ref-isbn
  :ensure org-ref)

(use-package org-ref-pdf
  :ensure org-ref)

(use-package outline
  :ensure t
  :hook (LaTeX-mode . outline-minor-mode))

(use-package outline-magic
  :ensure t
  :after outline
  :bind (:map outline-minor-mode-map ([f10] . outline-cycle)))

(use-package paren
  :ensure t
  :config
  (show-paren-mode t)
  :custom
  ;; highlight entire expression when on delimiters
  (show-paren-style 'expression))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

(use-package powerline
  :ensure t)

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
  :hook ((LaTeX-mode prog-mode) . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t)

(use-package reftex
  :ensure t
  :after latex
  :hook (LaTeX-mode . reftex-mode)
  :custom
  (reftex-default-bibliography '("~/Documents/papers/library.bib"))
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

(use-package sml-mode
  :ensure t
  :mode ("\\.sml\\'" . sml-mode))

(use-package solar
  :custom
  ;; Use 24 hour time in diary, calendar, etc.
  (calendar-time-display-form
   '(24-hours ":" minutes (if time-zone " (") time-zone (if time-zone ")")))
  (calendar-latitude 40.4)
  (calendar-longitude -79.9)
  (calendar-location-name "Pittsburgh, PA"))

(use-package spaceline
  :ensure t
  :requires spaceline-config)

(use-package tex
  :ensure auctex
  :custom
  (reftex-plug-into-AUCTeX t)
  (TeX-parse-self t) ; Parse documents to provide completion for packages, etc.
  (TeX-auto-save t) ; Automatically save style information
  (TeX-clean-confirm nil) ; Don't ask for confirmation when cleaning
  (TeX-source-correlate-mode t) ; Provide forward and inverse search with SyncTeX
  (TeX-source-correlate-method 'synctex)
  (TeX-PDF-mode t) ; use pdftex by default
  :config
  (add-to-list 'TeX-view-program-selection '(output-pdf "Zathura")))

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

(use-package twittering-mode
  :ensure t
  :custom
  (twittering-icon-mode t)
  (twittering-icon-storage-file "~/.emacs.d/storage/twittering-mode-icons.gz")
  (twittering-use-icon-storage t))

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

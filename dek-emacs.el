;; To remove the waiting time at startup...
;;
;; (modify-frame-parameters nil '((wait-for-wm . nil)))

(require 'cl)
(setq warning-suppress-types nil)
(setq frame-title-format '("" "Emacs - %b - %m"))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)
(add-to-list 'default-frame-alist '(background-mode . dark))

(powerline-default)
(load-theme 'zenburn t)

;;{{{ ;;;;;;;;;;;;;;;;;;;;;; CUA-MODE ;;;;;;;;;;;;;;;;;;;;;;

(setq-default transient-mark-mode t)
(setq-default cua-mode t)
(setq-default truncate-lines t)
(cua-mode t)
(global-set-key (kbd "C-<tab>") 'other-window)

;;}}}


;;{{{;;;;;;;;;;;;;;;;;; BYTE COMPILE ;;;;;;;;;;;;;;;;;;;;;;;

(defun dek-byte-compile-directory(directory)
  "Byte compile every .el file into a .elc file in the given
directory. See `byte-recompile-directory'."
  (interactive (list (read-file-name "Lisp directory: ")))
  (let (font-lock-verbose byte-compile-verbose)
    (setq font-lock-verbose nil)
    (setq byte-compile-verbose nil)
    (byte-recompile-directory directory 0 t))
  )

;;{{{;;;;;;;;;;;;;;;;;;;;;; LOADPATH ;;;;;;;;;;;;;;;;;;;;;;;

;; Set load path to emacd.d/site-lisp;;;;;;;;
(setq dek-compile-dest-dir nil) ;dont forget slash

;; (defun dek-compile-dest-file-function(el-file)
;;   (concat dek-compile-dest-dir (file-name-nondirectory el-file)))
;; (setq byte-compile-dest-file-function 'dek-compile-dest-file-function)

;; (if (equal user-login-name "dek")
;;     (add-to-list 'load-path dek-compile-dest-dir))

(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "~/.emacs.d/dek-lisp")

;;{{{;;;;;;;;;;;; BACKUP FILES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/Documents/.emacs-backups"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 8
   kept-old-versions 4
   version-control t)       ; use versioned backups

(defun force-backup-of-buffer ()
  (let ((buffer-backed-up nil))
    (backup-buffer)))
(add-hook 'before-save-hook  'force-backup-of-buffer)


;;{{{;;;;;;;;;;;;;; EMACS CLIENT STUFF ;;;;;;;;;;;;;;;;;;;;;

(add-hook 'server-switch-hook
	  (lambda ()
	    (when (current-local-map)
	      (use-local-map (copy-keymap (current-local-map))))
	    (when server-buffer-clients
	      (local-set-key (kbd "C-x k") 'server-edit))))


;;;;;;;;;;;;;;;;;; REOPEN BUFFER AS SUDO ;;;;;;;;;;;;;;;
(defun dek-rename-tramp-buffer ()
  (when (file-remote-p (buffer-file-name))
    (rename-buffer
     (format "%s:%s"
	     (file-remote-p (buffer-file-name) 'method)
	     (buffer-name)))))

(add-hook 'find-file-hook
	  'dek-rename-tramp-buffer)

(defun dek-reopen-file-sudo ()
  "Opens FILE with root privileges."
  (interactive)
  (let (tmp-buffer-file-name)
    (rename-buffer (concat (buffer-name nil) " [READ ONLY]"))
    (setq tmp-buffer-file-name buffer-file-name)
    (set-buffer (find-file (concat "/sudo::" tmp-buffer-file-name)))
   )
  )


;; Chrome Edit with emacs server
(if (equal user-login-name "dek")
    (lambda ()
      (require 'edit-server)
      (edit-server-start))
  (message "user is not dek ... chromium server not loaded")
)


;;{{{;;;;;;;;; FILE MANAGEMENT;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rename current file
(defun dek-rename-current-file-or-buffer ()
  (interactive)
  (if (not (buffer-file-name))
      (call-interactively 'rename-buffer)
    (let ((file (buffer-file-name)))
      (with-temp-buffer
	(set-buffer (dired-noselect file))
	(dired-do-rename)
	(kill-buffer nil))))
  nil)

;;{{{ ;;;;;;;;;;;;;;; VERSION CONTROL / GIT ;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-x V s") 'magit-status)
(global-set-key (kbd "C-x V l") 'magit-log)
(require 'helm-git)


;;{{{;;;;;;;;;;;; ZOOMING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'zoom-frm) ; ZOOMING
(global-set-key (if (boundp 'mouse-wheel-down-event) ; Emacs 22+
		    (vector (list 'control mouse-wheel-down-event))
		  [C-mouse-wheel])    ; Emacs 20, 21
		'zoom-in)
(when (boundp 'mouse-wheel-up-event) ; Emacs 22+
  (global-set-key (vector (list 'control mouse-wheel-up-event))
		  'zoom-out))


;;{{{;;;;;;;;;;; ANYTHING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/anything-config/")
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/anything-config/extensions/")
;; (require 'anything-startup)
;; (require 'anything-match-plugin)

;; (anything-read-string-mode 0)
;; (define-key anything-map (kbd "TAB") 'anything-next-source)
;; (define-key anything-map (kbd "M-x") 'anything-select-action)
;; (global-set-key (kbd "C-x f") 'anything-for-files)
;; (global-set-key (kbd "C-x y") 'anything-show-kill-ring)
;; (setq anything-enable-shortcuts t)


;;(setq split-width-threshold most-positive-fixnum) ;; used to be 160

(defun helm-for-git-files ()
  "DOCSTRING"
  (interactive)
  (if (magit-git-string "rev-parse" "--git-dir")
    (let (helm-for-files-preferred-list)
      (setq  helm-for-files-preferred-list
	       (quote (helm-c-source-ffap-line
		       helm-c-source-ffap-guesser
		       helm-c-source-buffers-list
		       helm-c-source-git-files
		       helm-c-source-recentf
		       helm-c-source-locate
		       helm-c-source-bookmarks
		       helm-c-source-file-cache)))
	(helm-for-files))
    (helm-for-files)))

(global-set-key (kbd "C-x f") 'helm-for-git-files)
(global-set-key (kbd "C-x y") 'anything-show-kill-ring)
(global-set-key (kbd "\C-x i") 'helm-browse-code)


;;{{{;;;;;;;;;; IDO-MODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-library "dek-ido")



;;{{{;; NICER MOVEMENT KEYBINDINGS, NAVIGATION ;;;;;;;;;;;;;

;; (autoload 'ergo-movement-mode "ergo-movement-mode.el" "for movement with \M-jkli")
;; (ergo-movement-mode 1)

(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)

(defun dek-end-of-line-then-return ()
  "End of line, then return"
  (interactive)
  (end-of-line)
  (reindent-then-newline-and-indent)
  )

(global-set-key [(control return)] 'dek-end-of-line-then-return)

(define-key key-translation-map [?\M-h] [?\C-b])
(define-key key-translation-map [?\M-l] [?\C-f])
(define-key key-translation-map [?\M-j] [?\C-n])
(define-key key-translation-map [?\M-k] [?\C-p])

(define-key key-translation-map (kbd "C-M-l") (kbd "C-M-f"))
(define-key key-translation-map (kbd "C-M-h") (kbd "C-M-b"))

;;;;;;;;;;;;;;; smooth scrolling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time at first
(setq smooth-scroll-margin 5)



;;;;;;;;;; Better Start of line ;;;;;;;;
(defun dek-back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))

(global-set-key (kbd "\C-a") 'dek-back-to-indentation-or-beginning)



;;;;;;;;;; Insert Line like vim ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "\C-o") '(lambda ()
				(interactive)
				(end-of-line)
				(newline-and-indent)))



;;{{{ ;;;;;;;;;;;;;;;;; ALIGN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dek-align-after-commas (beg end)
    (interactive "r")
    (align-regexp beg end ",\\(\\s-*\\)" 1 1 t))

(defun dek-fix-holder (beg end)
  (interactive "r")
  (beginning-of-buffer)
  (replace-regexp "(:,:,\\([12]\\))" "\\1"))

(defun dek-prune-table-to-one-member (beg end)
  (interactive "r")
  (beginning-of-buffer)
  (replace-regexp "^C:.*\n" "")
  (beginning-of-buffer)
  (replace-regexp "(:,:,\\([12]\\))" "\\1")
  (beginning-of-buffer)
  (replace-regexp "\\(.+?,.+?\\),.*" "\\1"))




;;{{{;;;;;;; COPYING AND KILLING ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\M-r" 'backward-kill-word)
(global-set-key "\C-\M-q" 'fill-paragraph)
;(global-set-key [(control \+)] 'dabbrev-expand)


;;;;;;;;; real copy behavior ;;;;;;;;;;;;;
(global-set-key "\M-v" 'cua-paste-pop)

(delete-selection-mode 1)






;;{{{;;;; AUTO-MARK, MARKS, BREADCRUMBS, BOOKMARKS ;;;;;;;;;

;; BREADCRUMBS
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/breadcrumbs")
;; (require 'dek-breadcrumbs)

(setq bookmark-default-file "~/.emacs.d/.emacs.bmk")
(setq bookmark-file "~/.emacs.d/.emacs.bmk")
(bookmark-load bookmark-default-file t)




;;{{{;;;;;;;;; LINUM GOTO LINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dek-goto-line ()
  "thisandthat."
  (interactive)
  (linum-mode 1)
  (let (userinput)
    (setq userinput (string-to-number (read-from-minibuffer "")))
    (if (eq userinput 0)
	(linum-mode -1)
      (progn
	(goto-line userinput)
	(linum-mode -1))
      )))
(global-set-key (kbd "M-g") 'dek-goto-line)



;;{{{;;;;;;;;; FILES AND BUFFERS;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-x\C-b" 'buffer-menu)


;;{{{;;;;;;;;;;;;; File manager/dired ;;;;;;;;;;;;;;;;;;;;

(require 'dired+)
(require 'dired-details)

(add-hook 'dired-load-hook
	  (lambda () (require 'dired-sort-menu+)))

(toggle-diredp-find-file-reuse-dir 1)

;;{{{;;;;;;;;;;; recent-files ;;;;;;;;;;;;;;;;;;;;;;;;
;(setq recentf-auto-cleanup 'never)
;; Save the recentf file list every 10 minutes (= 600 seconds)
(setq recentf-last-list '())
(setq recentf-max-saved-items 50)
(defun recentf-save-if-changes ()
  "Test if the recentf-list has changed and saves it in this case"
  (unless (equalp recentf-last-list recentf-list)
    (setq recentf-last-list recentf-list)
    (recentf-save-list)))
(run-at-time t 600 'recentf-save-if-changes)


(defun dek-open-recent-file ()
  "Goto your favorite place... which are defined in a list in
~/.emacs.d/favorite-places.el"
  (interactive)
  (let (ido-result-string dek-bookmark-list bookmark-prefix-string)
    (setq bookmark-prefix-string "bmk:")
    (setq dek-all-bookmark-names (bookmark-all-names))
    (dolist (tmp-bookmark-name (bookmark-all-names))
	    (setq dek-bookmark-list
		  (cons (concat bookmark-prefix-string tmp-bookmark-name) dek-bookmark-list)))
    ;; query ido for recent files and bookmarks
    (setq ido-result-string
	  (ido-completing-read "open recent or bookmarks:"
			       (append recentf-list dek-bookmark-list)
			       nil t))
    ;; take action according to result being a bookmark or a recent file
    (if (eql (search bookmark-prefix-string ido-result-string) 0)
	(bookmark-jump (substring ido-result-string (length bookmark-prefix-string)))
      (find-file ido-result-string)
      )
    )
  )

(global-set-key "\C-x\C-r" 'helm-recentf)



;;{{{;;;;;;;;; COMMENTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun comment-or-uncomment-current-line-or-region ()
  "Comments or uncomments current current line or whole lines in region."
  (interactive)
  (save-excursion
    (let (min max)
      (if (region-active-p)
	  (setq min (region-beginning) max (region-end))
	(setq min (point) max (point)))
      (comment-or-uncomment-region
       (progn (goto-char min) (line-beginning-position))
       (progn (goto-char max) (line-end-position))))))

(global-set-key (kbd "C-7") 'comment-or-uncomment-current-line-or-region)



;;{{{;;;;;;;;;;;;; MUTT EMAIL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun deks-mail-mode-hook ()
  (turn-on-auto-fill) ;;; Auto-Fill is necessary for mails
  (turn-on-font-lock) ;;; Font-Lock is always cool *g*
  (flush-lines "^\\(> \n\\)*> -- \n\\(\n?> .*\\)*") ;;; Kills quoted sigs.
  (not-modified) ;;; We haven't changed the buffer, haven't we? *g*
  (mail-text) ;;; Jumps to the beginning of the mail text
  (setq make-backup-files nil) ;;; No backups necessary.
  (define-key mail-mode-map "\C-c\C-c"
    '(lambda()
       (interactive)
       (save-buffer)
       (server-edit)
     ))
  )

(or (assoc "mutt-" auto-mode-alist)
    (setq auto-mode-alist
	  (cons '("mutt-" . mail-mode) auto-mode-alist)))

(add-hook 'mail-mode-hook 'deks-mail-mode-hook)



;;{{{;;;;;;;;;;;;;;; ISEARCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; searching ends at start of string always
(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
(defun my-goto-match-beginning ()
      (when (and isearch-forward (not isearch-mode-end-hook-quit))
	(goto-char isearch-other-end)))
(defadvice isearch-exit (after my-goto-match-beginning activate)
  "Go to beginning of match."
  (when isearch-forward (goto-char isearch-other-end)))



;;{{{;;;;;;;; BRACKET COMPLETION AND PAREN MATCHING ;;;;;;;;

;; (setq skeleton-pair t)
;; (defvar my-skeleton-pair-alist
;;   '((?\) . ?\()
;;     (?\] . ?\[)
;;     (?\} . ?\{)
;;     (?" . ?")))

;; (defun my-skeleton-pair-end (arg)
;;   "Skip the char if it is an ending, otherwise insert it."
;;   (interactive "*p")
;;   (let ((char last-command-event))
;;     (if (and (assq char my-skeleton-pair-alist)
;;	     (eq char (following-char)))
;;	(forward-char)
;;       (self-insert-command (prefix-numeric-value arg)))))

;; (defadvice backward-delete-char-untabify
;;   (before my-skeleton-backspace activate)
;;   "When deleting the beginning of a pair, and the ending is next char, delete it too."
;;   (let ((pair (assq (following-char) my-skeleton-pair-alist)))
;;     (and pair
;;	 (eq (preceding-char) (rest pair))
;;	 (delete-char 1))))

;; (dolist (pair my-skeleton-pair-alist)
;;   ;; Use backward delete function
;;   ; (global-set-key (char-to-string (first pair))
;;   ;   'my-skeleton-pair-end)
;;   ;; If the char for begin and end is the same,
;;   ;; use the original skeleton
;;   (global-set-key (char-to-string (rest pair))
;;		  'skeleton-pair-insert-maybe))

(require 'smartparens)
(smartparens-global-mode 1)
(show-smartparens-global-mode t)

(global-rainbow-delimiters-mode t)


;;{{{;;;;;;;;;;;;; YASNIPPET ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dek-find-yasnippet-dirname-in-list(filelist)
  "DOCSTRING"
  (interactive)
  (if (> (length filelist) 0)
      (if (or (= (length filelist) 1)
	      (string-match "yasnippet-[0-9]+" (car filelist)))
	  (car filelist)
	(dek-find-yasnippet-dirname-in-list (cdr filelist))
	)))



(setq yas/snippet-dirs '("~/.emacs.d/dek-lisp/snippets"))
(add-to-list
 'yas/snippet-dirs
 (concat
  package-user-dir "/"
  (dek-find-yasnippet-dirname-in-list (directory-files package-user-dir))
  "/snippets") t)

(yas-global-mode t)

;; old snippet definitions do not work anymore
;; (load-library "latex-snippets")
;; (load "latex-math-snippets")
;; (load "matlab-snippets")
;; (load "text-snippets")
;; (load "elisp-snippets")



;;{{{;;;;;;; AUTO-COMPLETE (AC-) ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

;; (setq-default ac-sources '(ac-source-yasnippet
;;			   ac-source-abbrev
;;			   ac-source-dictionary
;;			   ac-source-words-in-same-mode-buffers))
;; ;(define-key ac-menu-map (kbd "<f7>") 'ac-next)
;; (ac-set-trigger-key "TAB")
;; (global-set-key [(control \#)] 'auto-complete)
;; (define-key ac-completing-map (kbd "<RET>") 'ac-complete)
;; (define-key ac-completing-map (kbd "M-j") 'ac-next)
;; (define-key ac-completing-map (kbd "M-k") 'ac-previous)
;; (define-key ac-completing-map (kbd "C-n") 'ac-next)
;; (define-key ac-completing-map (kbd "C-p") 'ac-previous)
;; ;; (define-key ac-completing-map (kbd "<tab>") 'ac-next)
;; ;; (define-key ac-completing-map (kbd "<backtab>") 'ac-previous)

;; (add-to-list 'ac-modes 'latex-mode) ; auto-completion
;; (add-to-list 'ac-modes 'lua-mode) ; auto-completion
;; (add-to-list 'ac-modes 'matlab-mode) ; auto-completion
;; (add-to-list 'ac-modes 'conf-space-mode) ; auto-completion
;; (add-to-list 'ac-modes 'haskell-mode) ; auto-completion


;;{{{;;;;;;;;;;;; AUTO-INSERT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun my/autoinsert-yas-expand()
;;       "Replace text in yasnippet template."
;;       (yas/expand-snippet (buffer-string) (point-min) (point-max)))
;; (auto-insert-mode 1)
;; (setq auto-insert-directory "~/.emacs.d/auto-insert-templates/")
;; (setq auto-insert-alist '((("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header") . ["insert.h" c++-mode my/autoinsert-yas-expand])
;;			  (("\\.\\([C]\\|cc\\|cpp\\)\\'" . "C++ source") . ["insert.cc" my/autoinsert-yas-expand])
;;			  (("\\.sh\\'" . "Shell script") . ["insert.sh" my/autoinsert-yas-expand])
;;			  (("\\.el\\'" . "Emacs Lisp") . ["insert.el" my/autoinsert-yas-expand])
;;			  (("\\.pl\\'" . "Perl script") . ["insert.pl" my/autoinsert-yas-expand])
;;			  (("\\.pm\\'" . "Perl module") . ["insert.pm" my/autoinsert-yas-expand])
;;			  (("\\.py\\'" . "Python script") . ["insert.py" my/autoinsert-yas-expand])
;;			  (("[mM]akefile\\'" . "Makefile") . ["Makefile" my/autoinsert-yas-expand])
;;			  (("\\.tex\\'" . "TeX/LaTeX") . ["insert.tex" my/autoinsert-yas-expand])))

;; ;;;;;;;;;;;;;;;; JABBER/CHAT STUFF ;;;;;;;;;;

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-jabber-0.8.0/")
;; (load "jabber-autoloads")
;; (setq jabber-account-list
;;     '(("dk440241@googlemail.com"
;;        (:network-server . "talk.google.com")
;;        (:connection-type . ssl))))



;;{{{;;;;;;;;;;;; ORG MODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq  org-directory  "~/org")
(setq  org-default-notes-file  (concat  org-directory  "/TODO.org"))
;(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; Make TAB the yas trigger key in the org-mode-hook
(add-hook 'org-mode-hook
	  #'(lambda ()
	      (defvar yas/key-syntaxes (list "!_." "w" "w_.\\" "^ "))
	      (auto-fill-mode 0)
	      ))


(setq org-odd-levels-only t)
(setq org-hide-leading-stars t)


(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(define-key global-map "\C-ca" 'org-agenda)
;; (global-set-key "\C-cb" 'org-cycle-agenda-files) ;; redifined for bookmarks
(setq org-cycle-separator-lines 0)
(setq org-insert-heading-respect-content t)
(setq org-todo-keywords '((sequence "TODO" "STARTED" "DELEGATED" "|" "DONE" "DEFERRED" "CANCELLED")))

(setq org-tag-alist '(("rwth" . ?r) ("klausur" . ?k) ("organisation" . ?o)("LL" . ?l)("home" . ?h)("emacs" . ?e)("contact" . ?k)("theorie" .?t)("uebung" .?u)("zusammenfassung" .?z)("vorrechen" .?v)("current" . ?C)))

(setq org-file-apps (quote ((auto-mode . emacs) ("\\.x?html?\\'" . default) ("\\.pdf\\'" . "evince %s"))))
(setq org-insert-mode-line-in-empty-file t)
(setq org-display-custom-times nil)

; org mode logging
;(setq org-log-done nil)
(setq org-log-done 'time)
(setq org-log-note-clock-out t)

;; ORG-Agenda
(setq org-agenda-files (file-expand-wildcards "~/org/*.org")) ; setting agenda files
(add-to-list 'org-agenda-files  "~/Dropbox/AIA/showroom_neu/aia_showroom.org")
;; (if (equal user-login-name "dek")
;;     (load-file "~/bin/org-agenda/org-agenda-export.el")
;;   (message "dek is not the user ... external mashine ... org-agenda-export not loaded"))


;; ORG-remember Mode
;(org-remember-insinuate)  ;this apperantly doesn't work: so:
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(define-key  global-map  "\C-cr"  'org-remember)
(defvar dek-rwth-org-filename "rwth.org" "filename of rwth-org-file")
(defvar dek-rwth-org-filepath (concat "~/org/" dek-rwth-org-filename) "filepath to rwth-org-file")

(setq org-remember-templates
      '(("Todo" ?t "* TODO %?\n  %i\n  %a" "~/org/TODO.org" "Tasks")
	("system" ?s "* TODO %?\n  %i\n  %a" "~/org/system.org" "Tasks")
	("ll" ?l "* TODO %?\n  %i\n  %a" "~/org/liquid_lightning.org" "Tasks")
	("rwth" ?r "* TODO %?\n  %i\n  %a" dek-rwth-org-filepath "Tasks")))

;; ORG links:
(setq org-return-follows-link t)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-c\C-l" 'org-insert-link-global)
(global-set-key "\C-co" 'org-open-at-point-global)

; org -latex
(setq org-highlight-latex-fragments-and-specials t)

;; No ORG MODE STUFF after this

;; Orgmobile
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull "~/org/inbox.org")


;;{{{;;;;;;;;;;; PHP-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'php-mode "php-mode.el" "PHP editing mode" t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;; Toggle between PHP & HTML Helper mode.  Useful when working on
;; php files, that can been intertwined with HTML code

(add-hook 'php-mode-hook
	  (lambda()
	    (define-key php-mode-map [f5] 'html-mode)))
(add-hook 'html-mode-hook
	  (lambda()
	    (define-key html-mode-map [f5] 'php-mode)))

;;{{{;;;;;;;;;;; NXHTML-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nxhtml-mode-loader ()
  "thisandthat."
  (interactive)
  (load "nxhtml/autostart.el"))

;;{{{;;;;;;;;;;; XML-mode/YAML-mode ;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
;; (add-to-list 'auto-mode-alist '("\\.xml" . xml-mode))

;;{{{;;;;;;;;;;;;;;;;;; PYTHON ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dek-python-add-breakpoint ()
  (interactive)
  (let (pdb-regexp)
    (setq pdb-regexp "^\\s-*\\(import ipdb; ?\\)?ipdb.set_trace()")
  (if (string-match pdb-regexp (thing-at-point 'line))
      (kill-whole-line)
    (end-of-line)
    (newline-and-indent)
    (insert "import ipdb; ipdb.set_trace()")
    (highlight-lines-matching-regexp pdb-regexp)
    )))


(defun dek-python-find-all-breakpoints ()
  (interactive)
  (let (pdb-regexp point)
    (setq pdb-regexp "^\\s-*\\(import ipdb; ?\\)?ipdb.set_trace()$")
    (occur pdb-regexp)
    ))

(add-hook 'python-mode-hook '(lambda ()
			       (flycheck-mode 1)
			       (define-key python-mode-map (kbd "<f12>") 'dek-python-add-breakpoint)
			       (define-key python-mode-map (kbd "S-<f12>") 'dek-python-find-all-breakpoints)
			       ))

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args ""
      python-shell-prompt-regexp "In \\[[0-9]+\\]: "
      python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
      python-shell-completion-setup-code "from IPython.core.completerlib import module_completion" python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n" python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; JINJA2
(autoload 'jinja2-mode "jinja2-mode")



;;{{{;;;;;;;; LATEX AND AUCTEX  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.rwthtex$" . latex-mode))
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq preview-auto-cache-preamble t)
(setq reftex-plug-into-AUCTeX t)

(defun flymake-get-tex-args (file-name)
  (list "pdflatex" (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))

;(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
;(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook
	  '(lambda ()
	     (modify-syntax-entry ?\$ "$")
	     (tex-pdf-mode 1)
	     (auto-fill-mode t)
	     (setq TeX-auto-save t)
	     (setq TeX-parse-self t)
	     (setq ispell-enable-tex-parser t)
	     (flyspell-mode 1)
	     (LaTeX-math-mode t)
	     (local-set-key [tab] 'yas/expand)
	     (load-library "latex-commands")
	     (define-key LaTeX-mode-map (kbd "M-q") 'fill-sentence)
	     (define-key LaTeX-mode-map (kbd ".") 'end-fill-and-start-new-sentence)
	     (load-library "~/.emacs.d/dek-lisp/latex-snippets")
	     (load-library "~/.emacs.d/dek-lisp/latex-math-snippets")
	     ))

(add-hook 'TeX-mode-hook
	  '(lambda ()
	    (define-key TeX-mode-map (kbd "\C-c\C-c")
	      (lambda ()
		(interactive)
		(save-buffer)
		(TeX-command-menu "LaTeX")))
	    (define-key TeX-mode-map (kbd "<f12>")
	      (lambda ()
		(interactive)
		(TeX-view)
		[return]))))

(defun fill-sentence ()
  (interactive)
  (save-excursion
    (or (eq (point) (point-max)) (forward-char))
    (forward-sentence -1)
    (indent-relative t)
    (let ((beg (point))
	  (ix (string-match "LaTeX" mode-name)))
      (forward-sentence)
      (if (and ix (equal "LaTeX" (substring mode-name ix)))
	  (LaTeX-fill-region-as-paragraph beg (point))
	  (fill-region-as-paragraph beg (point))))))

(defun end-fill-and-start-new-sentence ()
  (interactive)
  (if  (string-match "[^a-zA-Z\(\)\{\}]" (char-to-string (char-before)))
      (insert ".")
    (fill-sentence)
    (insert ".")
    (reindent-then-newline-and-indent)
    )
  )



(setq LaTeX-math-abbrev-prefix "`")
(setq TeX-electric-escape nil)
;(setq TeX-fold-auto t)
(setq TeX-newline-function (quote reindent-then-newline-and-indent))
(setq TeX-fold-env-spec-list
       (quote
	(
	 (2 ("frame")
	 ("[comment]" ("comment"))
	 ))))


;(autoload 'whizzytex-mode "whizzytex"
;"WhizzyTeX, a minor-mode WYSIWIG environment for LaTeX" t)
;(setq-default whizzy-viewers '(("-pdf" "evince %s" )("-dvi" "evince %s")("-ps" "gv") ))

; rechtschreibung spellchecking aspell flyspell
(setq ispell-dictionary "english")
(setq ispell-local-dictionary "english")
(setq flyspell-default-dictionary "english")
(setq ispell-enable-tex-parser t)
(setq flyspell-issue-message-flag nil)

(defun dek-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
	 (change (if (string= dic "german") "english" "german")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)
    ))

(global-set-key (kbd "<f8>") 'dek-switch-dictionary)


;; (require 'dbus)
;; (defun th-evince-sync (file linecol)
;;    (let ((buf (get-buffer file))
;;          (line (car linecol))
;;          (col (cadr linecol)))
;;      (if (null buf)
;;          (message "Sorry, %s is not opened..." file)
;;        (switch-to-buffer buf)
;;        (goto-line (car linecol))
;;        (unless (= col -1)
;;          (move-to-column col)))))

;; (when (and
;;        (eq window-system 'x)
;;        (fboundp 'dbus-register-signal))
;;   (dbus-register-signal
;;    :session nil "/org/gnome/evince/Window/0"
;;    "org.gnome.evince.Window" "SyncSource"
;;    'th-evince-sync))


;;{{{;;;;;;;;;;;; C AND C++ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun my-turn-on-auto-newline ()
;;   (c-toggle-auto-newline 1))
;; (add-hook 'c-mode-common-hook 'my-turn-on-auto-newline)

(setq c-default-style "linux"
	   c-basic-offset 4)


(add-hook 'c++-mode-hook
	  (lambda ()
	    (unless (or (file-exists-p "makefile")
			(file-exists-p "Makefile"))
	      (set (make-local-variable 'compile-command)
		   (concat "make -k "
			   (file-name-sans-extension buffer-file-name))))))
(add-hook 'c++-mode-hook
	  '(lambda ()
	     ;(local-set-key "." 'semantic-complete-self-insert)
	     (setq compilation-finish-function
		   (lambda (buf str)
		     (if (string-match "exited abnormally" str)
			 ;;there were errors
			 (message "compilation errors, press C-x ` to visit")
		       ;;no errors:
		       ;; make the compilation window go away in 0.5 seconds
		       (run-at-time 1.0 nil 'delete-windows-on buf)
		       (message "NO COMPILATION ERRORS!")
		       (setq compilation-window-height 8))))))

;(define-key c++-mode-map "<f5>" 'compile)
;(define-key c++-mode-map (kbd "<f6>") 'gdb)
;(define-key c++-mode-map (kbd "<f7>") 'next-error)

;;{{{;;;;;;;;; c# and vb.net (Visual Basic);;;;;;;;;;;;;;;;;

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/csharp/")
;; (autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)


;; (setq auto-mode-alist
;;       (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;; (autoload 'vbnet-mode "vbnet-mode" "Mode for editing VB.NET code." t)
;; (setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vb\\)$" .
;;                                  vbnet-mode)) auto-mode-alist))

;; (defun my-vbnet-mode-fn ()
;;   "My hook for VB.NET mode"
;;   (interactive)
;;   ;; This is an example only.
;;   ;; These statements are not required to use VB.NET, but
;;   ;; you might like them.
;;   (turn-on-font-lock)
;;   (turn-on-auto-revert-mode)
;;   (setq indent-tabs-mode nil)
;;   (require 'flymake)
;;   (flymake-mode 1)
;;   )
;; (add-hook 'vbnet-mode-hook 'my-vbnet-mode-fn)

;;{{{;;;;;;;;;;;;;;;; JAVA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (global-set-key (kbd "<f5>") 'compile)

;; (defun java-run ()
;;   "thisandthat."
;;   (interactive)
;;   (compile (concat "java " (file-name-sans-extension buffer-file-name)))
;;   )

;(define-key java-mode-map "\C-c\C-v" 'java-run)

;; (defun java-open-brace ()
;;   "thisandthat."
;;   (interactive)
;;   (insert "{")
;;   (newline-and-indent)
;;     )

;; (setq auto-mode-alist
;;       (append '(("\\.java$" . java-mode)) auto-mode-alist))


;; (add-hook 'java-mode-hook
;;           (lambda ()
;;	    (define-key java-mode-map "\C-c\C-c" 'compile)
;;	    (define-key java-mode-map (kbd "RET") 'newline-and-indent)
;;	    (set (make-local-variable 'compile-command)
;;		 (concat "javac "
;;			 (buffer-file-name)
;;			 ;" && java "
;;			 ;(file-name-sans-extension buffer-file-name)
;;			 ))
;;	    (require 'java-docs)
;;	    ; replace docs lookup funktion with better one
;;	    (load-library "java-docs-dek-plus")
;;	    (java-docs-clear)
;;	    (java-docs "/usr/share/doc/openjdk-6-jdk/api")
;;	    (define-key java-mode-map "\C-cd" 'java-docs-lookup)
;;	    (define-key java-mode-map "{" 'java-open-brace)
;;	    (c-toggle-auto-hungry-state 1)
;;	    (c-toggle-auto-newline 1)
;;	    ))

;;{{{;;;;;;;;;;;;;;;; EMACS LISP ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'emacs-lisp-mode 'flycheck-mode)

;;{{{;;;;;;;;; OTHER MODES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;; haskell-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/haskell-mode-2.8.0/")
(load "haskell-site-file")
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; ;;;;;;;;;;;;;;;;;;;;;;; LUA MODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
;; (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
;; (add-to-list 'auto-mode-alist '("\\.lua.new$" . lua-mode))
;; (setq lua-indent-level 4)

;;;;;;;;;;;;;;; conf-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.*rc$" . conf-mode))

;;;;;;;;;;;;;;; instant messanging ;;;;;;;;;;
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/elim/elisp/")
;; (autoload 'garak "garak" nil t)

;;;;;;;;;;;;;;;;;;;;; csv-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'csv-mode "csv-mode" nil t)



;;{{{ ;;;;;;;;;;;;;;;;;;;;;;; MATLAB ;;;;;;;;;;;;;;;;;;;;;;;

;; Replace path below to be where your matlab.el file is.
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/matlab-emacs")
;; (load-library "matlab-load")
;; (message "matlab-load loaded")

(add-to-list 'load-path "~/.emacs.d/site-lisp/matlab")
(require 'matlab-load)

(setq matlab-shell-command-switches (quote ("-nodesktop" "-nosplash")))

;; Enable CEDET feature support for MATLAB code. (Optional)
; (matlab-cedet-setup)
;; (message "matlab-cedet loaded")
(if (equal user-login-name "davis")
    (setq matlab-shell-command "/pds/opt/matlab/bin/matlab"))

(defun dek-matlab-set-ssh (host)
  "Sends dbcont to matlab shell if you're in the matlab shell buffer"
  (interactive "sHost: ")
  (shell-command (concat "echo 'ssh -X davis@" host " matlab' > ~/bin/matlab_ssh"))
  (setq matlab-shell-command "~/bin/matlab_ssh")
  (message (concat "AIA Matlab host set to " host))
  )

(defun dek-matlab-set-breakpoint ()
  "thisandthat."
  (interactive)
  (let (line-number m-file-name command-string current-mfile-buffer)
    (setq line-number (number-to-string (line-number-at-pos)))
    (setq m-file-name (file-name-sans-extension buffer-file-name))
    (setq command-string (concat "dbstop in " m-file-name " at " line-number "\n"))
    (setq current-mfile-buffer (buffer-name))
    (matlab-show-matlab-shell-buffer)
    (matlab-shell-send-string command-string)
    (switch-to-buffer-other-window  current-mfile-buffer)
    )
  )

(defun dek-matlab-send-dbstep ()
  "thisandthat."
  (interactive)
  (matlab-shell-send-string "dbstep\n")
  )

(defun dek-matlab-send-dbcont ()
  "Sends dbcont to matlab shell if you're in the matlab shell buffer"
  (interactive)
  (matlab-shell-send-string "dbcont\n")
  )

(add-hook 'matlab-shell-mode-hook
	  '(lambda ()
	     (define-key matlab-shell-mode-map (kbd "C-<up>") 'windmove-up)
	     (define-key matlab-shell-mode-map (kbd "C-<down>") 'windmove-down)
	     (define-key matlab-shell-mode-map (kbd "<f5>") 'dek-matlab-send-dbcont)
	     (define-key matlab-shell-mode-map (kbd "<f11>") 'dek-matlab-send-dbstep)
	     ))

(add-hook 'matlab-mode-hook
	  '(lambda ()
	     (define-key matlab-mode-map (kbd "<f12>") 'dek-matlab-set-breakpoint)
	     ))


(defun mfindent ()
  (interactive)
  (let (rectstart)
    (re-search-forward "^function.*\n\\(%.*\n\\)*")
    (setq rectstart (point))
    (re-search-forward "\\(\\([[:space:]]+.*\\)?\n\\)*?end")
    (if (y-or-n-p "Do it?")
	(progn
	  (insert " ")
	  (replace-rectangle rectstart (point) "")))))

(message "MATLAB ALL LOADED!!!")


;;{{{;;;;;;;;;;;;;;;;;;;;;;;;;; STUMPWM ;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'stumpwm-mode)
;; (add-to-list 'auto-mode-alist '("\\.stumpwmrc$" . stumpwm-mode))

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;{{{;;;;;;;;;;;;; FONT AND SETUP ;;;;;;::::::;;;;;;;;;;;;;;

(setq
 visible-bell t ;; turn on visual bell
 inhibit-startup-screen t
 ; scalable-fonts-allowed t
 column-number-mode t
 require-final-newline t ; make newline at end of file)
 ;; vc-handled-backends nil
 )

(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

(fset 'yes-or-no-p 'y-or-n-p) ;; Use "y or n" answers instead of full words "yes or no"
(global-font-lock-mode t)
(blink-cursor-mode 1)
(tool-bar-mode -1)
(fringe-mode '(1 . 0))
(setq fringes-outside-margins t)
(setq font-lock-maximum-decoration (quote ((dired-mode) (t . t)))) ; apperantly adds nice colors

(setq scroll-bar-mode-explicit t)
(set-scroll-bar-mode `right)

;; PRINTING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq lpr-command "gtklp")

;; BROWSER
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium")

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'prog-mode-hook 'whitespace-turn-off t)
(add-hook 'python-mode-hook 'whitespace-turn-off t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fci-rule-color "#2b2b2b")
 '(fill-column 80)
 '(flymake-no-changes-timeout 1.5)
 '(global-semantic-decoration-mode t)
 '(global-semantic-highlight-func-mode t)
 '(py-shell-switch-buffers-on-execute nil)
 '(python-shell-interpreter "ipython")
 '(virtualenv-root "~/.virtualenvs/"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3f3f3f" :foreground "#dcdccc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "unknown" :family "Monaco"))))
 '(flymake-errline ((t (:inherit nil :background "#483131" :foreground "*" :underline nil :weight bold))))
 '(flymake-warnline ((t (:background "#366060" :foreground "#e0cf9f" :underline nil :weight bold))))
 '(fringe ((t (:background "#4f4f4f" :foreground "#dcdccc" :weight normal :height 0.3 :width condensed))))
 '(mode-line ((t (:background "#506070" :foreground "#dcdccc" :box (:line-width -1 :style released-button) :family "Ubuntu Condensed"))))
 '(mode-line-inactive ((t (:foreground "#808080" :background "#666666" :box nil))))
 '(semantic-tag-boundary-face ((t (:overline "#93e0e3"))) t))





;;{{{;;;;;;;;;;;;;;;;;;; AUTO-RECOMPILE ;;;;;;;;;;;;;;;;;;;;

;; (defun byte-compile-user-init-file ()
;;       (let ((byte-compile-warnings '(unresolved)))
;;         ;; in case compilation fails, don't leave the old .elc around:
;;         (when (file-exists-p (concat user-init-file ".elc"))
;;           (delete-file (concat user-init-file ".elc")))
;;         (byte-compile-file user-init-file)
;;         ;; (message "%s compiled" user-init-file)
;;         ))
;;     (defun my-emacs-lisp-mode-hook ()
;;       (when (equal buffer-file-name user-init-file)
;;         (add-hook 'after-save-hook 'byte-compile-user-init-file t t)))
;;     ;; (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
;;     (add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)


;;{{{;;;;;;;;;;CUSTOM FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun .emacs ()
  "switch to my emacs file"
  (interactive)
  (if (get-buffer "~/.emacs.d/dek-emacs.el")
      (switch-to-buffer "~/.emacs.d/dek-emacs.el")
    (find-file "~/.emacs.d/dek-emacs.el")
    )
  )

(defun rwth ()
  "switch to my rwth org file"
  (interactive)
  (if (get-buffer dek-rwth-org-filename)
      (switch-to-buffer dek-rwth-org-filename)
      (find-file dek-rwth-org-filepath)
      )
  )


(defun ld ()
  "load last directory in dired"
  (interactive)
  (find-file-existing (shell-command-to-string "cat ~/.ld|head -c -1"))
  )

(defun sd ()
  "switch to current directory by creating new window in tmux"
  (interactive)
  (concat "echo " "'" (file-name-directory (buffer-file-name)) "' > ~/.ld" )
  (shell-command "tmux neww")
  )


;;; FOR WHATEVER PROJECT YOUR WORDKING ON ;;;;;;;;;;;;;;;;
(setq yas/triggers-in-field t)

(setq tetris-score-file "~/.emacs.d/.tetris-scores")

(defun dek-set-system-dependant-default-font(fontlist)
  "DOCSTRING"
  (if (>= (length fontlist) 2)
      (let (tmpsystem tmpfont tmpfontheight)
	(setq tmpsystem (car fontlist)
	      tmpfont (cadr fontlist)
	      tmpfontheight (caddr fontlist))
	(if (equal system-name tmpsystem)
	    (set-face-attribute 'default nil :family tmpfont :height tmpfontheight)
	  (dek-set-system-dependant-default-font (cddr fontlist)))
	)))

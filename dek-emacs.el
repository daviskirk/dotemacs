;;; .emacs --- Davis Kirkendall
;;
;;; Commentary:
;; This is the EMACS init file for Davis Kirkendall
;; Sections are divided by comment bars.
;;
;;; Code:

(require 'cl)

(if (equal user-login-name "dek")
    (setenv "PATH"
	    (concat
	     "/usr/local/texlive/2013/bin/x86_64-linux/:"
	     (expand-file-name "~/anaconda/bin") ":"
	     (getenv "PATH"))))

(defun .emacs ()
  "Switch to my emacs file."
  (interactive)
  (if (get-buffer (expand-file-name "dek-emacs.el" user-emacs-directory))
      (switch-to-buffer (expand-file-name "dek-emacs.el" user-emacs-directory))
    (find-file (expand-file-name "dek-emacs.el" user-emacs-directory))
    )
  )

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(set-face-attribute 'default nil :height 98 :family "Monaco")

;; (setq debug-on-error t)
(setq frame-title-format '("" "Emacs - %b - %m"))

;; (add-to-list 'default-frame-alist '(background-mode . dark))

;;;;;;;;;;;;;;;;;;;;;;;;; LOADPATH ;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory ))
(add-to-list 'load-path (expand-file-name "dek-lisp" user-emacs-directory ))
;;;;;;;;;;;;;;;;;;;;;;;; PACKAGE ;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'setup-package)

(require 'powerline)
(powerline-default-theme)
(load-theme 'zenburn t)

(global-rainbow-delimiters-mode 1)
;; (show-paren-mode 1)
;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;;;;;;;;;;;;;;;;;;;;;;;;; CUA-MODE ;;;;;;;;;;;;;;;;;;;;;;
(setq-default transient-mark-mode t)
(setq-default cua-mode t)
(setq-default truncate-lines t)
(cua-mode t)

(setq sublimity-scroll-vdecc 1.3)
(setq sublimity-scroll-vspeeds '(5000 1000 500 200 100 50 10 5))
(setq sublimity-auto-hscroll nil)
(setq sublimity-scroll-hdecc 1)
(require 'sublimity-scroll)

;;;;;;;;;;;;;;;;;; KEY-CHORD ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(key-chord-mode 1)
(key-chord-define-global "xf" 'helm-for-files)
(key-chord-define-global "xb" 'helm-for-files)
(key-chord-define-global "xs" 'save-buffer)

(key-chord-define-global "xx" 'cua-cut-region)
(key-chord-define-global "cc" 'cua-copy-region)
(key-chord-define-global "vv" (kbd "C-v"))
(setq key-chord-two-keys-delay 0.05)

;;;;;;;;;;;;;;;;;; MULTIPLE-CURSORS ;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'multiple-cursors)
;; (global-unset-key (kbd "C-m"))
(global-set-key (kbd "M-m") 'mc/mark-next-like-this)
(global-set-key (kbd "M-S-m") 'mc/mark-all-like-this)
(global-set-key (kbd "C-M-m") 'mc/mark-all-in-region)
(global-set-key (kbd "C-M-<return>") 'mc/edit-lines)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)


;;;;;;;;;;;;;;;;;;;;; WINDOWS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key   (kbd "\C-x w w") 'helm-swap-windows)
(winner-mode 1)
(setq winner-mode 1)

(global-set-key (kbd "C-1") 'toggle-delete-other-windows)
(key-chord-define-global "x2" 'split-window-below)
(key-chord-define-global "x3" 'split-window-right)

(defun toggle-delete-other-windows ()
  (interactive)
  (if (> (length (window-list)) 1)
      (delete-other-windows)
    (winner-undo)))

;;;;;;;;;;;;;;;;;;;;; BYTE COMPILE ;;;;;;;;;;;;;;;;;;;;;;;

(defun dek-byte-compile-directory(directory)
  "Byte compile every .el file into a .elc file in the given
directory. See `byte-recompile-directory'."
  (interactive (list (read-file-name "Lisp directory: ")))
  (let (font-lock-verbose byte-compile-verbose)
    (setq font-lock-verbose nil)
    (setq byte-compile-verbose nil)
    (byte-recompile-directory directory 0 t))
  )

;;;;;;;;;;;;;;; BACKUP FILES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(global-set-key (kbd "<f9>") 'save-buffer)

;;;;;;;;;;;;;;;;;;;; save place ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;;;;;;;;;;;;;;;;;; SSH / TRAMP ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq tramp-default-method "ssh")

;;;;;;;;;;;;;;;;; EMACS CLIENT STUFF ;;;;;;;;;;;;;;;;;;;;;

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
    (when (and (require 'edit-server nil t) (daemonp))
      (edit-server-start))
  (message "user is not dek ... chromium server not loaded")
)


;;;;;;;;;;;; FILE MANAGEMENT;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
	(error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
	(if (get-buffer new-name)
	    (error "A buffer named '%s' already exists!" new-name)
	  (rename-file filename new-name 1)
	  (rename-buffer new-name)
	  (set-visited-file-name new-name)
	  (set-buffer-modified-p nil)
	  (message "File '%s' successfully renamed to '%s'"
		   name (file-name-nondirectory new-name)))))))

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;;;;;;;;;;;;;;;;;;;;;; AUTO-SAVE;;;;;;;;;;;;;;;;;;;;;;;;
(defun save-buffer-if-visiting-file (&optional args)
  "Save the current buffer only if it is visiting a file"
  (interactive)
  (if (buffer-file-name)
      (save-buffer args)))

;; This causes files that I'm editing to be saved automatically by the
;; emacs auto-save functionality.  I'm hoping to break myself of the
;; c-x c-s twitch.
(add-hook 'auto-save-hook 'save-buffer-if-visiting-file)

(setq auto-save-timeout 4)
(setq auto-save-interval 4000)
(auto-save-mode 1)

;;;;;;;;;;;;;;;;;; VERSION CONTROL / GIT ;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-x V s") 'magit-status)
(global-set-key (kbd "C-x V l") 'magit-log)
(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(defun add-magit-toggle-whitespace-key
  (interactive)
  (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace))

(add-hook 'magit-mode 'add-magit-toggle-whitespace-key)


;;;;;;;;;;;;;;; ZOOMING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'zoom-frm) ; ZOOMING
(global-set-key (if (boundp 'mouse-wheel-down-event) ; Emacs 22+
	    (vector (list 'control mouse-wheel-down-event))
	  [C-mouse-wheel])    ; Emacs 20, 21
	'zoom-in)
(when (boundp 'mouse-wheel-up-event) ; Emacs 22+
  (global-set-key (vector (list 'control mouse-wheel-up-event))
	  'zoom-out))

;;;;;;;;;;;;;;;;; HELM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'helm-themes)
(global-set-key (kbd "C-x f") 'helm-for-files)
(global-set-key (kbd "C-x y") 'anything-show-kill-ring)

(global-set-key (kbd "\C-x i") 'helm-imenu)


;;;;;;;;;;;;; IDO-MODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-library "dek-ido")

;;;;; NICER MOVEMENT KEYBINDINGS, NAVIGATION ;;;;;;;;;;;;;
(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)

(define-key key-translation-map [?\M-h] [?\C-b])
(define-key key-translation-map [?\M-l] [?\C-f])
(define-key key-translation-map [?\M-j] [?\C-n])
(define-key key-translation-map [?\M-k] [?\C-p])

(define-key key-translation-map (kbd "C-M-l") (kbd "C-M-f"))
(define-key key-translation-map (kbd "C-M-h") (kbd "C-M-b"))
(key-chord-define-global "fg"  'iy-go-to-char)
(key-chord-define-global "fd"  'iy-go-to-char-backward)
(global-set-key "\M-." 'iy-go-to-char)
(global-set-key "\M-," 'iy-go-to-char-backward)
(key-chord-define-global "fs" 'ace-jump-mode)

(global-set-key (kbd "C-M-SPC") 'er/expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(defadvice pop-to-mark-command (around ensure-new-position activate)
  "Continue popping mark until the cursor moves.
Also, if the last command was a copy - skip past all the
expand-region cruft."
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

;;;;;;;;;;;;;;;;; smart operator ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'smart-operator)
(defun my-matlab-mode-smart-operator-hook()
  (smart-insert-operator-hook)
  (local-unset-key (kbd "."))
  (local-unset-key (kbd ":"))
  (local-unset-key (kbd ","))
  (local-unset-key (kbd "*"))
  (local-unset-key (kbd "/"))
  (local-unset-key (kbd "%"))
  )
(defun my-python-mode-smart-operator-hook()
  (smart-insert-operator-hook)
  (local-unset-key (kbd "."))
  (local-unset-key (kbd ":"))
  (local-unset-key (kbd "*"))
  (local-unset-key (kbd "/"))
  (define-key python-mode-map "="
    '(lambda ()
       (interactive)
       (if (looking-back "([^)]+[^ ]")
	   (self-insert-command 1)
	 (smart-insert-operator "="))))
  )


(add-hook 'matlab-mode-hook 'my-matlab-mode-smart-operator-hook)
(add-hook 'python-mode-hook 'my-python-mode-smart-operator-hook)


;;;;;;;;;;;;;;; smooth scrolling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq scroll-margin 10
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)

;;;;;;;;;; Better Start of line ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dek-back-to-indentation-or-beginning ()
  "Go to indentation or to the beginning of the line."
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


;;;;;;;;;;;;;;;;;;;; ALIGN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (replace-regexp "\\(.+?,.+?\\),.*" "\\1"))




;;;;;;;;;; COPYING AND KILLING ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key "\M-r" 'backward-kill-word)
(global-set-key "\C-\M-q" 'fill-paragraph)

(key-chord-define-global "kk" 'kill-whole-line)

;;;;;;;;; real copy behavior ;;;;;;;;;;;;;
(global-set-key "\M-v" 'cua-paste-pop)
(delete-selection-mode 1)

(auto-indent-global-mode)
(setq auto-indent-known-indent-level-variables
      '( c-basic-offset lisp-body-indent sgml-basic-offset))



;;;;;;; AUTO-MARK, MARKS, BREADCRUMBS, BOOKMARKS ;;;;;;;;;

;; BREADCRUMBS
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/breadcrumbs")
;; (require 'dek-breadcrumbs)

(setq bookmark-default-file (expand-file-name ".emacs.bmk" user-emacs-directory))
(setq bookmark-file (expand-file-name ".emacs.bmk" user-emacs-directory))
(bookmark-load bookmark-default-file t)


;;;;;;;;;;;; LINUM GOTO LINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
	(linum-mode 1)
	(goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(global-set-key (kbd "M-g") 'goto-line-with-feedback)


;;;;;;;;;;;; FILES AND BUFFERS;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-x\C-b" 'buffer-menu)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)


;;;;;;;;;;;;;;;; File manager/dired ;;;;;;;;;;;;;;;;;;;;

(require 'dired+)
(require 'dired-details)
(setq dired-details-hidden-string "- ")
(dired-details-install)
(define-key dired-mode-map "(" 'dired-details-toggle)
(define-key dired-mode-map ")" 'dired-details-toggle)


(add-hook 'dired-load-hook
      (lambda () (require 'dired-sort-menu+)))

(toggle-diredp-find-file-reuse-dir 1)

;; let end of buffer and start of buffer move to last/first file
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))
(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)
(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)


;;;;;;;;;;;;;; recent-files ;;;;;;;;;;;;;;;;;;;;;;;;
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

(global-set-key "\C-x\C-r" 'helm-recentf)



;;;;;;;;;;;; COMMENTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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



;;;;;;;;;;;;;;;; MUTT EMAIL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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



;;;;;;;;;;;;;;;;;; ISEARCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; searching ends at start of string always
(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
(defun my-goto-match-beginning ()
      (when (and isearch-forward (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))
(defadvice isearch-exit (after my-goto-match-beginning activate)
  "Go to beginning of match."
  (when isearch-forward (goto-char isearch-other-end)))


;;;;;;;;;;;;;;;;;;;;; autopair ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'autopair)
;; (autopair-global-mode -1) ;; to enable in all buffers

;;;;;;;;;;;;;;;;;;;;; smartparens ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)

;;;;;;;;;;;;;;;; YASNIPPET ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dek-find-elpa-yasnippet-snippet-dir ()
  (interactive)
  (concat
   package-user-dir "/"
   (car (directory-files package-user-dir nil "^yasnippet-[0-9.]+"))
   "/snippets"))

(setq yas-snippet-dirs
      (list (expand-file-name "dek-lisp/yasnippet-snippets" user-emacs-directory)
	(dek-find-elpa-yasnippet-snippet-dir)))

(yas-global-mode t)

;;;;;;;;;; AUTO-COMPLETE (AC-) ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

;; (setq-default ac-sources '(ac-source-yasnippet
;;             ac-source-abbrev
;;             ac-source-dictionary
;;             ac-source-words-in-same-mode-buffers))
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

(add-to-list 'ac-modes 'latex-mode) ; auto-completion
(add-to-list 'ac-modes 'lua-mode) ; auto-completion
(add-to-list 'ac-modes 'matlab-mode) ; auto-completion
(add-to-list 'ac-modes 'conf-space-mode) ; auto-completion
(add-to-list 'ac-modes 'haskell-mode) ; auto-completion

;;;;;;;;;;;;;;; AUTO-INSERT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/autoinsert-yas-expand()
      "Replace text in yasnippet template."
      (yas-expand-snippet (buffer-string) (point-min) (point-max)))
(auto-insert-mode 1)
(setq auto-insert-directory (expand-file-name "auto-insert-templates/" user-emacs-directory))
(setq auto-insert-alist
      '(
	;; (("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header") . ["insert.h" c++-mode my/autoinsert-yas-expand])
	;; (("\\.\\([C]\\|cc\\|cpp\\)\\'" . "C++ source") . ["insert.cc" my/autoinsert-yas-expand])
	;; (("\\.sh\\'" . "Shell script") . ["insert.sh" my/autoinsert-yas-expand])
	;; (("\\.el\\'" . "Emacs Lisp") . ["insert.el" my/autoinsert-yas-expand])
	;; (("\\.pl\\'" . "Perl script") . ["insert.pl" my/autoinsert-yas-expand])
	;; (("\\.pm\\'" . "Perl module") . ["insert.pm" my/autoinsert-yas-expand])
	(("\\.py\\'" . "Python script") . ["insert.py" my/autoinsert-yas-expand])
	;; (("[mM]akefile\\'" . "Makefile") . ["Makefile" my/autoinsert-yas-expand])
	;; (("\\.tex\\'" . "TeX/LaTeX") . ["insert.tex" my/autoinsert-yas-expand])
	))

;; ;;;;;;;;;;;;;;;; JABBER/CHAT STUFF ;;;;;;;;;;

;; (add-to-list 'load-path (expand-file-name "site-lisp/emacs-jabber-0.8.0/" user-emacs-directory))
;; (load "jabber-autoloads")
;; (setq jabber-account-list
;;     '(("dk440241@googlemail.com"
;;        (:network-server . "talk.google.com")
;;        (:connection-type . ssl))))


;;;;;;;;;;;;;; Diminish modeline clutter ;;;;;;;;;;;;;;;;;;
(require 'diminish)
(diminish 'autopair-mode)


;;;;;;;;;;;;;;; ORG MODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-startup-folded t)
(setq  org-directory  "~/org")
(setq  org-default-notes-file  (expand-file-name org-directory "TODO.org"))
;(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; Make TAB the yas trigger key in the org-mode-hook
(add-hook 'org-mode-hook
      #'(lambda ()
	  (defvar yas/key-syntaxes (list "!_." "w" "w_.\\" "^ "))
	  (auto-fill-mode 0)
	  (auto-indent-mode -1)
          (define-key org-mode-map (kbd "C-<tab>") 'other-window)
	  ))

(setq org-odd-levels-only nil)
(setq org-hide-leading-stars t)


(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(define-key global-map "\C-ca" 'org-agenda)
;; (global-set-key "\C-cb" 'org-cycle-agenda-files) ;; redifined for bookmarks
(setq org-cycle-separator-lines 0)
(setq org-insert-heading-respect-content t)
(setq org-todo-keywords '((sequence "TODO" "DOING" "BLOCKED" "REVIEW" "|" "DONE" "ARCHIVED")))
;; Setting Colours (faces) for todo states to give clearer view of work
(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
        ("DOING" . "#F0DFAF") ;; yellow
        ("BLOCKED" . "#CC9393") ;; red
        ("REVIEW" . "#8CD0D3") ;; blue
        ("DONE" . org-done)
        ("ARCHIVED" . "#8C5353")))

(setq org-tag-alist '(("rwth" . ?r) ("klausur" . ?k) ("organisation" . ?o)("LL" . ?l)("home" . ?h)("emacs" . ?e)("contact" . ?k)("theorie" .?t)("uebung" .?u)("zusammenfassung" .?z)("vorrechen" .?v)("current" . ?C)))

(setq org-file-apps (quote ((auto-mode . emacs) ("\\.x?html?\\'" . default) ("\\.pdf\\'" . "evince %s"))))
(setq org-insert-mode-line-in-empty-file t)
(setq org-display-custom-times nil)

; org mode logging
;(setq org-log-done nil)
(setq org-log-done 'time)
(setq org-log-note-clock-out t)

;; ORG-Agenda
(setq org-agenda-files (file-expand-wildcards "~/Documents/Code/aise/aia.org")) ; setting agenda files
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

;;;;;;;;;;;;;; PHP-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;; NXHTML-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nxhtml-mode-loader ()
  "thisandthat."
  (interactive)
  (load "nxhtml/autostart.el"))

;;;;;;;;;;;;;; XML-mode/YAML-mode ;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
;; (add-to-list 'auto-mode-alist '("\\.xml" . xml-mode))

;;;;;;;;;;;;;;;;;;;;; RST MODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'rst-mode-hook '(lambda ()
			    (flycheck-mode 1)
                            (auto-indent-mode -1)
                            (setq-local auto-indent-kill-line-at-eol nil)
                            (setq-local auto-indent-on-yank-or-paste nil)
                            (define-key rst-mode-map (kbd "RET") 'newline-and-indent)
                            ))

;;;;;;;;;;;;;;;;;;;;; PYTHON ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun dek-python-crunch ()
  "Comment region if region is active, have 2 spaces for inline comments."
  (interactive)
  (if (region-active-p)
      (comment-region (point) (mark))
    (when (and (looking-at "$") (not (looking-back "^\\|\\([[:space:]]\\{2\\}\\)")))
      (just-one-space 2))
    (insert "#")))

(add-hook 'python-mode-hook
	  '(lambda ()
	     (flycheck-mode 1)
	     (auto-indent-mode -1)
             (setq-local auto-indent-kill-line-at-eol nil)
             (setq-local auto-indent-on-yank-or-paste nil)
             (define-key python-mode-map (kbd "RET") 'newline-and-indent)
             (define-key python-mode-map (kbd "#") 'dek-python-crunch)
	     (define-key python-mode-map (kbd "<f12>") 'dek-python-add-breakpoint)
	     (define-key python-mode-map (kbd "S-<f12>") 'dek-python-find-all-breakpoints)
             (setq paragraph-start "\\(\\s-*$\\)\\|\\(\\.$)")
             (setq paragraph-start "\f\\|\\(\s-*$\\)\\|\\([-:] +.+$\\)" paragraph-seperate "$")
             ))

;; jedi mode
(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)                 ; optional
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'jedi-mode-hook
	  '(lambda ()
	     (define-key jedi-mode-map (kbd "<C-tab>") nil)
	     (define-key jedi-mode-map (kbd "<backtab>") 'jedi:complete)))


(setq
 python-shell-interpreter "~/anaconda/bin/ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))")

;; JINJA2
(autoload 'jinja2-mode "jinja2-mode")
(add-to-list 'auto-mode-alist '("\\.jinja2$" . jinja2-mode))

;;;;;;;;;;; LATEX AND AUCTEX  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.rwthtex$" . TeX-latex-mode))
(add-to-list 'auto-mode-alist '("\\.tex$" . TeX-latex-mode))
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
	 ;; (load-library "latex-commands")
	 (define-key LaTeX-mode-map (kbd "M-q") 'fill-sentence)
	 (define-key LaTeX-mode-map (kbd "<tab>") 'LaTeX-indent-line)
	 ;; (load-library (expand-file-name "dek-lisp/latex-snippets" user-emacs-directory))
	 ;; (load-library (expand-file-name "dek-lisp/latex-math-snippets" user-emacs-directory))
	 (key-chord-define LaTeX-mode-map ". "  ".\C-j")
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
  (fill-sentence)
  (insert ".")
  (reindent-then-newline-and-indent)
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

;;;;;;;;;;;;;;;;;;;;; PROGRAMMING MODES ;;;;;;;;;;;;;;;
(add-hook 'after-init-hook 'global-flycheck-mode)
(setq flycheck-flake8rc (expand-file-name ".flake8rc" user-emacs-directory))
(setq prog-mode-hooks
      '(matlab-mode
	python-mode
	latex-mode
	TeX-latex-mode
	emacs-lisp-mode))
;; add prog-mode-hook to all programming language modes
(dolist (tmp-prog-mode-hook prog-mode-hooks nil)
  (add-hook tmp-prog-mode-hook (lambda () (run-hooks 'prog-mode-hook))))

;;;;;;;;;;;;;;;;;;;;;;;; FLYSPELL ;;;;;;;;;;;;;;;;;;;;;;;;;
; rechtschreibung spellchecking aspell flyspell
(setq ispell-program-name "aspell")
(setq ispell-extra-args '("--sug-mode=ultra"))
(setq ispell-dictionary "english")
(setq ispell-local-dictionary "english")
(setq flyspell-default-dictionary "english")
(setq ispell-enable-tex-parser t)
(setq flyspell-issue-message-flag nil)

(add-hook 'prog-mode-hook
	  (lambda ()
            (flyspell-prog-mode)
	    (set-face-attribute 'flyspell-incorrect nil :foreground "#ac736f" :weight 'bold)
	    (set-face-attribute 'flyspell-duplicate nil :foreground "#8c836f" :underline t)))

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


;;;;;;;;;;;;;;; C AND C++ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;; c# and vb.net (Visual Basic);;;;;;;;;;;;;;;;;

;; (add-to-list 'load-path (expand-file-name "site-lisp/csharp/" user-emacs-directory))
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

;;;;;;;;;;;;;;;;;;; JAVA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;      (define-key java-mode-map "\C-c\C-c" 'compile)
;;      (define-key java-mode-map (kbd "RET") 'newline-and-indent)
;;      (set (make-local-variable 'compile-command)
;;       (concat "javac "
;;           (buffer-file-name)
;;           ;" && java "
;;           ;(file-name-sans-extension buffer-file-name)
;;           ))
;;      (require 'java-docs)
;;      ; replace docs lookup funktion with better one
;;      (load-library "java-docs-dek-plus")
;;      (java-docs-clear)
;;      (java-docs "/usr/share/doc/openjdk-6-jdk/api")
;;      (define-key java-mode-map "\C-cd" 'java-docs-lookup)
;;      (define-key java-mode-map "{" 'java-open-brace)
;;      (c-toggle-auto-hungry-state 1)
;;      (c-toggle-auto-newline 1)
;;      ))

;;;;;;;;;;;;;;;;;;; EMACS LISP ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;; clojure mode ;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'clojure-mode-hook 'paredit-mode)

;;;;;;;;;;;; OTHER MODES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;; haskell-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; (add-to-list 'load-path (expand-file-name "site-lisp/elim/elisp/" user-emacs-directory))
;; (autoload 'garak "garak" nil t)

;;;;;;;;;;;;;;;;;;;;; csv-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'csv-mode "csv-mode" nil t)



;;;;;;;;;;;;;;;;;;;;;;;;;; MATLAB ;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "site-lisp/matlab" user-emacs-directory))
(require 'matlab-load)

(setq matlab-shell-command-switches (quote ("-nodesktop" "-nosplash")))

;; Enable CEDET feature support for MATLAB code. (Optional)
; (matlab-cedet-setup)
;; (message "matlab-cedet loaded")
(if (equal user-login-name "davis")
    (setq matlab-shell-command "/pds/opt/matlab/bin/matlab")
  (setq matlab-shell-command "~/opt/matlab/bin/matlab"))

(defun dek-matlab-set-ssh (host)
  "Set matlab binary to matlab binary on HOST over ssh."
  (interactive "sHost: ")
  (shell-command (concat "echo 'ssh -X davis@" host " matlab' > ~/bin/matlab_ssh"))
  (setq matlab-shell-command "~/bin/matlab_ssh")
  (message (concat "AIA Matlab host set to " host))
  )

(defun dek-matlab-set-breakpoint ()
  "Set breakpoint in matlab."
  (interactive)
  (let (line-number m-file-name command-string current-mfile-buffer)
    (setq line-number (number-to-string (line-number-at-pos)))
    (setq m-file-name (file-name-sans-extension buffer-file-name))
    (setq command-string (concat "dbstop in '" m-file-name "' at " line-number "\n"))
    (setq current-mfile-buffer (buffer-name))
    (matlab-show-matlab-shell-buffer)
    (matlab-shell-send-string command-string)
    (switch-to-buffer-other-window  current-mfile-buffer)
    )
  )

(defun dek-matlab-goto-error-line ()
  "Get last error line, switch buffer and go to that line."
  (interactive)
  (let (errline original-pos)
    (setq original-pos (point))
    (search-backward-regexp "(line [0-9]*)")
    (search-forward-regexp "[0-9]")
    (setq errline (thing-at-point 'number))
    (goto-char original-pos)
    (other-window 1)
    (goto-line errline)
    ))

(defun dek-matlab-send-dbstep ()
  "Send dbstep to matlab buffer."
  (interactive)
  (matlab-shell-send-string "dbstep\n")
  )

(defun dek-matlab-send-dbcont ()
  "Sends dbcont to matlab shell if you're in the matlab shell buffer."
  (interactive)
  (matlab-shell-send-string "dbcont\n")
  )

(add-hook 'matlab-shell-mode-hook
      '(lambda ()
	 (define-key matlab-shell-mode-map (kbd "C-<up>") 'windmove-up)
	 (define-key matlab-shell-mode-map (kbd "C-<down>") 'windmove-down)
	 (define-key matlab-shell-mode-map (kbd "<f5>") 'dek-matlab-send-dbcont)
	 (define-key matlab-shell-mode-map (kbd "<f11>") 'dek-matlab-send-dbstep)
         (define-key matlab-shell-mode-map (kbd "C-l") 'erase-buffer)
         (define-key matlab-shell-mode-map (kbd "C-c <tab>") 'dek-matlab-goto-error-line)
	 ))

(add-hook 'matlab-mode-hook
	  '(lambda ()
	     (require 'matlab-expansions)
	     (auto-complete-mode 1)
	     (define-key matlab-mode-map (kbd "<f12>") 'dek-matlab-set-breakpoint)
	     (key-chord-define matlab-mode-map ";;"  "\C-e;")
	     ))

;; fast delimiters
(key-chord-define-global
 "((" '(lambda ()
	 (interactive)
	 (insert "(")
	 (forward-sexp)
	 (insert ")")
	 (forward-char)
	 ))

(key-chord-define-global
 "[[" '(lambda ()
	 (interactive)
	 (insert "[")
	 (forward-sexp)
	 (insert "]")
	 (forward-char)
	 ))

(key-chord-define-global
 "{{" '(lambda ()
	 (interactive)
	 (insert "[")
	 (forward-sexp)
	 (insert "]")
	 (forward-char)
	 ))

(key-chord-define-global
 "ww" 'switch-window)

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
;; (message "MATLAB ALL LOADED!!!")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; STUMPWM ;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'stumpwm-mode)
;; (add-to-list 'auto-mode-alist '("\\.stumpwmrc$" . stumpwm-mode))


;;;;;;;;;;;;;;;; FONT AND SETUP ;;;;;;::::::;;;;;;;;;;;;;;

(setq
 visible-bell t ;; turn on visual bell
 inhibit-startup-screen t
 ; scalable-fonts-allowed t
 column-number-mode t
 require-final-newline t ; make newline at end of file)
 ;; vc-handled-backends nil
 )

;; (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
;; (global-fci-mode 1)


(fset 'yes-or-no-p 'y-or-n-p) ;; Use "y or n" answers instead of full words "yes or no"
(global-font-lock-mode t)
(blink-cursor-mode 1)
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


(require 'virtualenv)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex")
 '(LaTeX-command-style (quote (("" "%(PDF)%(latex) %S%(PDFout)"))))
 '(ansi-term-color-vector [unspecified "#282a2e" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#81a2be" "#e0e0e0"])
 '(auto-indent-on-visit-pretend-nothing-changed nil)
 '(custom-safe-themes (quote ("1cf3f29294c5a3509b7eb3ff9e96f8e8db9d2d08322620a04d862e40dc201fe2" "cd70962b469931807533f5ab78293e901253f5eeb133a46c2965359f23bfb2ea" "769bb56fb9fd7e73459dcdbbfbae1f13e734cdde3cf82f06a067439568cdaa95" "253bd40645913cc95b9f8ef0533082cb9a4cb0810f854c030f3ef833ee5b9731" "1f31a5f247d0524ef9c051d45f72bae6045b4187ed7578a7b1f8cb8758f92b60" default)))
 '(fci-rule-color "#2b2b2b")
 '(fill-column 79)
 '(flycheck-check-syntax-automatically (quote (save new-line mode-enabled)))
 '(flycheck-idle-change-delay 2)
 '(flymake-no-changes-timeout 1.5)
 '(fortran-do-indent 2)
 '(fortran-if-indent 2)
 '(global-semantic-decoration-mode t)
 '(global-semantic-highlight-func-mode t)
 '(jedi:key-complete [backtab])
 '(matlab-case-level (quote (4 . 4)))
 '(matlab-fill-code nil)
 '(matlab-shell-command-switches (quote ("-nodesktop" "-nosplash")))
 '(switch-window-shortcut-style (quote qwerty))
 '(test-case-python-executable "~/anaconda/bin/python")
 '(virtualenv-root "~/.virtualenvs/")
 '(warning-suppress-types (quote ((undo discard-info)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monaco"))))
 '(flymake-errline ((t (:inherit nil :background "#483131" :foreground "*" :underline nil :weight bold))) t)
 '(flymake-warnline ((t (:background "#366060" :foreground "#e0cf9f" :underline nil :weight bold))) t)
 '(fringe ((t (:background "#4f4f4f" :foreground "#dcdccc" :weight normal :height 0.3 :width condensed))))
 '(mode-line ((t (:background "#506070" :foreground "#dcdccc" :box (:line-width -1 :style released-button) :family "Ubuntu Condensed"))))
 '(mode-line-inactive ((t (:background "#555555" :foreground "#808080" :box nil :family "Ubuntu Condensed"))))
 '(semantic-tag-boundary-face ((t (:overline "#93e0e3"))) t)
 '(sp-show-pair-match-face ((t (:background "#2F2F2F" :weight bold)))))





;;;;;;;;;;;;;;;;;;;;;; AUTO-RECOMPILE ;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;CUSTOM FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun rwth ()
  "Switch to my rwth org file."
  (interactive)
  (if (get-buffer dek-rwth-org-filename)
      (switch-to-buffer dek-rwth-org-filename)
      (find-file dek-rwth-org-filepath)
      )
  )

(defun ld ()
  "Load last directory in dired."
  (interactive)
  (find-file-existing (shell-command-to-string "cat ~/.ld|head -c -1"))
  )

(defun sd ()
  "Switch to current directory by creating new window in tmux."
  (interactive)
  (concat "echo " "'" (file-name-directory (buffer-file-name)) "' > ~/.ld" )
  (shell-command "tmux neww")
  )


;;; FOR WHATEVER PROJECT YOUR WORDKING ON ;;;;;;;;;;;;;;;;
(setq yas/triggers-in-field t)

(setq tetris-score-file (expand-file-name ".tetris-scores" user-emacs-directory))

(defun dek-set-system-dependant-default-font(fontlist)
  "Set system dependent font. TODO: implement this correctly."
  (if (>= (length fontlist) 2)
      (let (tmpsystem tmpfont tmpfontheight)
    (setq tmpsystem (car fontlist)
	  tmpfont (cadr fontlist)
	  tmpfontheight (caddr fontlist))
    (if (equal system-name tmpsystem)
	(set-face-attribute 'default nil :family tmpfont :height tmpfontheight)
      (dek-set-system-dependant-default-font (cddr fontlist)))
    )))
(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

(provide 'dek-emacs)
;;; dek-emacs.el ends here

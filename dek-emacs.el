;;; .emacs --- Davis Kirkendall
;;
;;; Commentary:
;; This is the EMACS init file for Davis Kirkendall
;; Sections are divided by comment bars.
;;
;;; Code:

(require 'cl)
(require 'epa-file)
(epa-file-enable)
(setenv "GPG_AGENT_INFO" nil)

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
(menu-bar-mode 0)
(set-face-attribute 'default nil :height 98 :family "Monaco")
;; (setq debug-on-error t)
(setq frame-title-format '("" "Emacs - %b - %m"))

;; (add-to-list 'default-frame-alist '(background-mode . dark))

;;;;;;;;;;;;;;;;;;;;;;;;; LOADPATH ;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory ))
(add-to-list 'load-path (expand-file-name "dek-lisp" user-emacs-directory ))

;;;;;;;;;;;;;;;;;;;;;;;; PACKAGE ;;;;;;;;;;;;;;;;;;;;;;;;;
(defun package-desc-vers (a)
  nil)

;; (setq package-enable-at-startup nil)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("MELPA" . "http://melpa.org/packages/")
	("org" . "http://orgmode.org/elpa/")))
(package-initialize)

(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)

(use-package powerline
 :config (powerline-default-theme))

(load-theme 'zenburn t)

;; (show-paren-mode 1)
;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;;;;;;;;;;;;;;;;;;;;;;;;; CUA-MODE ;;;;;;;;;;;;;;;;;;;;;;
(setq-default transient-mark-mode t)
(setq-default cua-mode t)
(setq-default truncate-lines t)
(cua-mode t)

;; (setq sublimity-scroll-vdecc 1.3)
;; (setq sublimity-scroll-vspeeds '(5000 1000 500 200 100 50 10 5))
;; (setq sublimity-auto-hscroll nil)
;; (setq sublimity-scroll-hdecc 1)
;; (require 'sublimity-scroll)

;;;;;;;;;;;;;;;;;; KEY-CHORD ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package key-chord-mode
  :commands (key-chord-define-global key-chord-mode)
  :init
  (setq key-chord-two-keys-delay 0.001)
  (setq key-chord-one-key-delay 0.15)
  )
(key-chord-mode 1)
(key-chord-define-global "xf" 'helm-for-files)
(key-chord-define-global "xb" 'ido-switch-buffer)
(key-chord-define-global "xs" 'save-buffer)

(key-chord-define-global "xx" 'cua-cut-region)
(key-chord-define-global "cc" 'cua-copy-region)
(key-chord-define-global "vv" (kbd "C-v"))
(key-chord-define-global "aa" (kbd "C-a"))
(key-chord-define-global "ee" 'move-end-of-line)

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

(key-chord-define-global
 "w2" 'dek-current-buffer-to-other-window)

;;;;;;;;;;;;;;;;;; MULTIPLE-CURSORS ;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'multiple-cursors)
;; (global-unset-key (kbd "C-m"))
(bind-key "M-m" 'mc/mark-next-like-this)
(bind-key "M-S-m" 'mc/mark-all-like-this)
(bind-key "C-M-m" 'mc/mark-all-in-region)
(bind-key "C-M-<return>" 'mc/edit-lines)
(unbind-key "M-<down-mouse-1>")
(bind-key "M-<mouse-1>" 'mc/add-cursor-on-click)


;;;;;;;;;;;;;;;;;;;;; WINDOWS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bind-key "C-<tab>" 'other-window)
(bind-key "C-x o" 'switch-window)
(winner-mode 1)
(setq winner-mode 1)

(bind-key "C-1" 'toggle-delete-other-windows)
(key-chord-define-global "x1" 'toggle-delete-other-windows)
(key-chord-define-global "x2" 'split-window-below)
(key-chord-define-global "x3" 'split-window-right)

(use-package buffer-move
  :bind (("C-x w <M-up>" . buf-move-up)
         ("C-x w <M-down>" . buf-move-down)
         ("C-x w <M-left>" . buf-move-left)
         ("C-x w <M-right>" . buf-move-right)))

(use-package windmove
  :bind (("\C-x w <up>" . windmove-up)
         ("\C-x w <down>" . windmove-down)
         ("\C-x w <left>" . windmove-left)
         ("\C-x w <right>" . windmove-right)))

(defun toggle-delete-other-windows ()
  (interactive)
  (if (> (length (window-list)) 1)
      (delete-other-windows)
    (winner-undo)))

(defun dek-current-buffer-to-other-window ()
  (interactive)
  (let (buf)
    (setq buf (current-buffer))
    (other-window 1)
    (switch-to-buffer buf)
    (other-window -1))
  )

;;;;;;;;;;;;;;;;;;;;;; COMPILING ;;;;;;;;;;;;;;;;;;;;;;;;
(bind-key "<f5>" 'compile)

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
(bind-key "<f9>" 'save-buffer)

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
(use-package magit
  :commands (magit-status magit-log magit-dont-ignore-whitespace)
  :bind (("C-x V s" . magit-status)
	 ("C-x V l" . magit-log))
  :defer t
  :init
  (defun magit-toggle-whitespace ()
    (interactive)
    (if (member "--ignore-space-change" magit-diff-options)
        (magit-dont-ignore-whitespace)
      (magit-ignore-whitespace)))

  (defun magit-ignore-whitespace ()
    (interactive)
    (add-to-list 'magit-diff-options "--ignore-space-change")
    (message "ignoring whitespace")
    (magit-refresh))

  (defun magit-dont-ignore-whitespace ()
    (interactive)
    (setq magit-diff-options (remove "--ignore-space-change" magit-diff-options))
    (message "paying attention to whitespace")
    (magit-refresh))
  :config
  (bind-key "W" 'magit-toggle-whitespace magit-status-mode-map))

;;;;;;;;;;;;;;; ZOOMING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package zoom-frm
  :commands (zoom-in zoom-out)
  :init
  (global-set-key (if (boundp 'mouse-wheel-down-event) ; Emacs 22+
                      (vector (list 'control mouse-wheel-down-event))
                    [C-mouse-wheel])    ; Emacs 20, 21
                  'zoom-in)
  (when (boundp 'mouse-wheel-up-event) ; Emacs 22+
    (global-set-key (vector (list 'control mouse-wheel-up-event))
                    'zoom-out))
  )

;;;;;;;;;;;;;;;;; PROJECTILE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t))

;;;;;;;;;;;;;;;;; HELM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package helm
  :bind (("C-x w w" . helm-swap-windows)
         ("C-x f" . helm-for-files)
         ("C-x y" . helm-show-kill-ring)
         ("C-x i" . helm-imenu))
  :config
  (if (not (boundp 'helm-source-projectile-files-list))
      (setq helm-source-projectile-files-list '()))

  (defun dek-helm-for-files ()
    "Use projectile with Helm instead of ido."
    (interactive)
    (helm :sources '(helm-source-projectile-files-list
                     helm-source-projectile-recentf-list
                     helm-source-projectile-buffers-list
                     helm-source-buffers-list
                     helm-source-recentf
                     helm-source-locate)))

  (defun dek-helm-browse-code (regexp)
    (interactive "s")
    (setq helm-multi-occur-buffer-list (list (buffer-name (current-buffer))))
    (helm-occur-init-source)
    (helm :sources 'helm-source-occur
          :buffer "*helm occur*"
          :preselect (and (memq 'helm-source-occur helm-sources-using-default-as-input)
                          (format "%s:%d:" (buffer-name) (line-number-at-pos (point))))
          :input regexp
          :truncate-lines t))
  )

(use-package helm-themes)
(use-package helm-files)
(use-package helm-swoop
  :bind ("M-i" . helm-swoop)
  :config
  (setq helm-swoop-pre-input-function (lambda () nil))
  )


;;;;;;;;;;;;; IDO-MODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-library "dek-ido")

;; (defun ivy-alt (&optional arg)
;;   "Exit the minibuffer with the selected candidate.
;; When ARG is t, exit with current text, ignoring the candidates."
;;   (interactive "P")
;;   (let (dir)
;;     (cond ((and ivy--directory
;; 		(or
;; 		 (and
;; 		  (not (string= ivy--current "./"))
;; 		  (cl-plusp ivy--length)
;; 		  (file-directory-p
;; 		   (setq dir (expand-file-name
;; 			      ivy--current ivy--directory))))))
;; 	   (ivy--cd dir)
;; 	   (ivy--exhibit))
;; 	  ((string-match "^/\\([^/]+?\\):\\(?:\\(.*\\)@\\)?" ivy-text)
;; 	   (let ((method (match-string 1 ivy-text))
;; 		 (user (match-string 2 ivy-text))
;; 		 res)
;; 	     (dolist (x (tramp-get-completion-function method))
;; 	       (setq res (append res (funcall (car x) (cadr x)))))
;; 	     (setq res (delq nil res))
;; 	     (when user
;; 	       (dolist (x res)
;; 		 (setcar x user)))
;; 	     (setq res (cl-delete-duplicates res :test 'equal))
;; 	     (let ((host (ivy-read "Find File: "
;; 				   (mapcar #'ivy-build-tramp-name res))))
;; 	       (when host
;; 		 (setq ivy--directory "/")
;; 		 (ivy--cd (concat "/" method ":" host ":"))))))
;; 	  (t
;; 	   (ivy-next-line)))))

;; (require 'smex)
;; (use-package swiper
;;   :config
;;   (ivy-mode t)
;;   (setq smex-completion-method 'ivy)
;;   ;; (bind-key "TAB" 'ivy-partial ivy-minibuffer-map)
;;   (bind-key "TAB" 'ivy-alt ivy-minibuffer-map))

;;;;; NICER MOVEMENT KEYBINDINGS, NAVIGATION ;;;;;;;;;;;;;
(bind-key "RET" 'reindent-then-newline-and-indent)

(define-key key-translation-map [?\M-h] [?\C-b])
(define-key key-translation-map [?\M-l] [?\C-f])
(define-key key-translation-map [?\M-j] [?\C-n])
(define-key key-translation-map [?\M-k] [?\C-p])

(define-key key-translation-map (kbd "C-M-l") (kbd "C-M-f"))
(define-key key-translation-map (kbd "C-M-h") (kbd "C-M-b"))
(key-chord-define-global "fg"  'iy-go-to-char)
(key-chord-define-global "fd"  'iy-go-to-char-backward)
(bind-key "M-." 'iy-go-to-char)
(bind-key "M-," 'iy-go-to-char-backward)
(key-chord-define-global "fs" 'ace-jump-mode)
(key-chord-define-global "kk" 'kill-whole-line)
(bind-key "M-SPC" 'cycle-spacing)

;;;;;;;;;;;;;;;;;;; expand region
(use-package expand-region
  :bind (("C-M-SPC" . er/expand-region)
	 ("C-=" . er/expand-region)))

;; (global-set-key (kbd "C-M-SPC") 'er/expand-region)
;; (global-set-key (kbd "C-=") 'er/expand-region)

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
(use-package smart-operator
  :ensure t)
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
;; (defun my-python-mode-smart-operator-hook()
;;   (smart-insert-operator-hook)
;;   (local-unset-key (kbd "."))
;;   (local-unset-key (kbd ":"))
;;   (local-unset-key (kbd "*"))
;;   (local-unset-key (kbd "/"))
;;   (define-key python-mode-map "="
;;     '(lambda ()
;;        (interactive)
;;        (if (looking-back "([^)]+[^ ]")
;; 	   (self-insert-command 1)
;; 	 (smart-insert-operator "="))))
;;   )

(add-hook 'matlab-mode-hook 'my-matlab-mode-smart-operator-hook)
;; (add-hook 'python-mode-hook 'my-python-mode-smart-operator-hook)


;; ;; smooth scrolling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq scroll-margin 500
;;       scroll-conservatively 1000
;;       scroll-up-aggressively 0.0
;;       scroll-down-aggressively 0.0
;;       auto-window-vscroll nil)

;;;;;;;;;; Better Start of line ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dek-back-to-indentation-or-beginning ()
  "Go to indentation or to the beginning of the line."
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))

(bind-key "C-a" 'dek-back-to-indentation-or-beginning)



;;;;;;;;;; Insert Line like vim ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bind-key "C-o" '(lambda ()
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

(defun align-repeat (start end regexp)
  "Repeat alignment with respect to
     the given regular expression."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end
                (concat "\\(\\s-*\\)" regexp) 1 1 t))



;;;;;;;;;; COPYING AND KILLING ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-key "M-r" 'backward-kill-word)
(bind-key "C-M-q" 'fill-paragraph)

;;;;;;;;; real copy behavior ;;;;;;;;;;;;;
(bind-key "M-v" 'cua-paste-pop)
(delete-selection-mode 1)

(use-package auto-indent-global-mode
  :config
  (auto-indent-global-mode)
  (setq auto-indent-known-indent-level-variables
        '( c-basic-offset lisp-body-indent sgml-basic-offset)))

;;;;;;;;;;;; LINUM GOTO LINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
	(linum-mode 1)
	(goto-line (read-number "Goto line: ")))
    (linum-mode -1)))
(bind-key "M-g" 'goto-line-with-feedback)


;;;;;;;;;;;; FILES AND BUFFERS;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bind-key "C-x C-b" 'buffer-menu)

;; Add parts of each file's directory to the buffer name if not unique
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

;;;;;;;;;;;;;;;; File manager/dired ;;;;;;;;;;;;;;;;;;;;

(use-package dired+)
(use-package dired-details
  :config
  (setq dired-details-hidden-string "- ")
  (dired-details-install)
  ;; (define-key dired-mode-map "(" 'dired-details-toggle)
  ;; (define-key dired-mode-map ")" 'dired-details-toggle)
  )
(require 'dired+)
(require 'dired-details)


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
(setq recentf-max-saved-items 100)

(defun recentf-save-if-changes ()
  "Test if the recentf-list has changed and saves it in this case"
  (unless (equalp recentf-last-list recentf-list)
    (setq recentf-last-list recentf-list)
    (recentf-save-list)))
(run-at-time t 600 'recentf-save-if-changes)

(bind-key "C-x C-r" 'helm-recentf)



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

(bind-key "C-7" 'comment-or-uncomment-current-line-or-region)



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


;;;;;;;;;; AUTO-COMPLETE (AC-) ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ac-modes '())
;; (require 'auto-complete)
;; (require 'auto-complete-config)
;; (ac-config-default)

;; ;; (setq-default ac-sources '(ac-source-yasnippet
;; ;;             ac-source-abbrev
;; ;;             ac-source-dictionary
;; ;;             ac-source-words-in-same-mode-buffers))
;; ;; ;(define-key ac-menu-map (kbd "<f7>") 'ac-next)
;; ;; (ac-set-trigger-key "TAB")
;; ;; (bind-key "C-#" 'auto-complete)
;; ;; (define-key ac-completing-map (kbd "<RET>") 'ac-complete)
;; ;; (define-key ac-completing-map (kbd "M-j") 'ac-next)
;; ;; (define-key ac-completing-map (kbd "M-k") 'ac-previous)
;; ;; (define-key ac-completing-map (kbd "C-n") 'ac-next)
;; ;; (define-key ac-completing-map (kbd "C-p") 'ac-previous)
;; ;; ;; (define-key ac-completing-map (kbd "<tab>") 'ac-next)
;; ;; ;; (define-key ac-completing-map (kbd "<backtab>") 'ac-previous)

;; (add-to-list 'ac-modes 'latex-mode) ; auto-completion
;; (add-to-list 'ac-modes 'lua-mode) ; auto-completion
;; (add-to-list 'ac-modes 'matlab-mode) ; auto-completion
;; (add-to-list 'ac-modes 'conf-space-mode) ; auto-completion
;; (add-to-list 'ac-modes 'haskell-mode) ; auto-completion
;;;;;;;;;;;;;;;; YASNIPPET ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yasnippet
  :commands (yas-global-mode yas-minor-mode)
  :ensure t
  :diminish yas-minor-mode
  :init
  (defun dek-find-elpa-yasnippet-snippet-dir ()
    (interactive)
    (concat
     package-user-dir "/"
     (car (directory-files package-user-dir nil "^yasnippet-[0-9.]+"))
     "/snippets"))
  (defvar dek-yasnippet-dir
    (expand-file-name "dek-lisp/yasnippet-snippets" user-emacs-directory))
  (setq yas-snippet-dirs
	(list dek-yasnippet-dir
	      (dek-find-elpa-yasnippet-snippet-dir)))
  (yas-global-mode 1)
  )


;;;;;;;;;;;;;;;; COMPANY-MODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :commands (company-complete tab-indent-or-complete company-manual-begin)
  :init
  (defun indent-or-complete ()
    (interactive)
    (if (looking-at "\\_>")
	(condition-case nil
	    (company-complete-common)
	  (error (indent-according-to-mode)))
      (indent-according-to-mode)))

  (defun company-complete-common-or-previous-cycle ()
  "Insert the common part of all candidates, or select the next one."
  (interactive)
  (when (company-manual-begin)
    (let ((tick (buffer-chars-modified-tick)))
      (call-interactively 'company-complete-common)
      (when (eq tick (buffer-chars-modified-tick))
        (let ((company-selection-wrap-around t))
          (call-interactively 'company-select-previous))))))

    (defun check-expansion ()
    (save-excursion
      (if (looking-at "\\_>") t
	(backward-char 1)
	(if (looking-at "\\.") t
	  (backward-char 1)
	  (if (looking-at "->") t nil)))))

  (defun do-yas-expand ()
    (let ((yas/fallback-behavior 'return-nil))
      (yas/expand)))

  (defun tab-indent-or-complete ()
    (interactive)
    (cond
     ((minibufferp)
      (minibuffer-complete))
     (t
      (indent-for-tab-command)
      (if (or (not yas-minor-mode)
	      (null (do-yas-expand)))
	  (if (check-expansion)
	      (progn
		(company-manual-begin)
		(if (null company-candidates)
		    (progn
		      (company-abort)
		      (indent-for-tab-command)))))))))

  ;; (bind-key [tab] 'tab-indent-or-complete)
  (bind-key "<tab>" 'tab-indent-or-complete prog-mode-map)
  ;; (bind-key [(control return)] 'company-complete-common)

  :ensure t
  :config
  (global-company-mode)
  (bind-key "C-n" 'company-select-next-or-abort company-active-map)
  (bind-key "C-p" 'company-select-previous-or-abort company-active-map)
  (add-to-list 'company-backends 'company-anaconda)

  (defun tab-complete-or-next-field ()
    (interactive)
    (if (or (not yas-minor-mode)
	    (null (do-yas-expand)))
	(if company-candidates
	    (company-complete-selection)
	  (if (check-expansion)
	      (progn
		(company-manual-begin)
		(if (null company-candidates)
		    (progn
		      (company-abort)
		      (yas-next-field))))
	    (yas-next-field)))))

  (defun expand-snippet-or-complete-selection ()
    (interactive)
    (if (or (not yas-minor-mode)
	    (null (do-yas-expand))
	    (company-abort))
	(company-complete-common-or-cycle)))

  (defun abort-company-or-yas ()
    (interactive)
    (if (null company-candidates)
	(yas-abort-snippet)
      (company-abort)))

  (bind-key "<tab>" 'expand-snippet-or-complete-selection company-active-map)
  (bind-key "<backtab>" 'company-complete-common-or-previous-cycle company-active-map)

  (unbind-key "<tab>" yas-minor-mode-map)
  (bind-key "<tab>" 'tab-complete-or-next-field yas-keymap)
  (bind-key "C-<tab>" 'yas-next-field yas-keymap)
  (bind-key "C-g" 'abort-company-or-yas yas-keymap)
  )

;; (use-package company-jedi
;;   :config
;;   (defun dek-python-company-mode-hook ()
;;     (add-to-list 'company-backends 'company-jedi))
;;   (add-hook 'python-mode-hook 'dek-company-mode-hook)
;;   )



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
(setq org-html-head-extra "<link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap.min.css\"><link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap-theme.min.css\"><script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.1/js/bootstrap.min.js\"></script><body style=\"margin-left:15%;margin-right:15%;\">")

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(bind-key "C-c a" 'org-agenda)
;; (bind-key "C-c b" 'org-cycle-agenda-files) ;; redifined for bookmarks
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
(bind-key  "C-c r"  'org-remember)
(defvar dek-rwth-org-filename "rwth.org" "filename of rwth-org-file")
(defvar dek-rwth-org-filepath (concat "~/org/" dek-rwth-org-filename) "filepath to rwth-org-file")

(setq org-remember-templates
      '(("Todo" ?t "* TODO %?\n  %i\n  %a" "~/org/TODO.org" "Tasks")
    ("system" ?s "* TODO %?\n  %i\n  %a" "~/org/system.org" "Tasks")
    ("ll" ?l "* TODO %?\n  %i\n  %a" "~/org/liquid_lightning.org" "Tasks")
    ("rwth" ?r "* TODO %?\n  %i\n  %a" dek-rwth-org-filepath "Tasks")))

;; ORG links:
(setq org-return-follows-link t)
(bind-key "C-c l" 'org-store-link)
(bind-key "C-c C-l" 'org-insert-link-global)
(bind-key "C-c o" 'org-open-at-point-global)

;; Include the latex-exporter
(require 'ox-latex)
;; Add minted to the defaults packages to include when exporting.
(add-to-list 'org-latex-packages-alist '("" "minted"))
;; Tell the latex export to use the minted package for source
;; code coloration.
(setq org-latex-listings 'minted)

;; No ORG MODE STUFF after this

;; Orgmobile
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull "~/org/inbox.org")

;;;;;;;;;;;;;;; ORG BABEL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t) (sh . t)))

;;;;;;;;;;;;;;;;; ORG publish ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-publish-project-alist
      '(

        ("org-daviskirk"
         ;; Path to your org files.
         :base-directory "~/Documents/Code/daviskirk.github.io/org/"
         :base-extension "org"

         ;; Path to your Jekyll project.
         :publishing-directory "~/Documents/Code/daviskirk.github.io/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :html-extension "html"
         :body-only t ;; Only export section between <body> </body>
         )


        ("org-static-daviskirk"
         :base-directory "~/Documents/Code/daviskirk.github.io/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
         :publishing-directory "~/Documents/Code/daviskirk.github.io/"
         :recursive t
         :publishing-function org-publish-attachment)

        ("daviskirk" :components ("org-daviskirk" "org-static-daviskirk"))

        ))


;;;;;;;;;;;;;;;;; MARKDOWN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode)
(use-package gh-md
  :defer t
  :config
  (bind-key "C-c C-c c" 'gh-md-render-buffer markdown-mode-map)
  (bind-key "<f5>" 'gh-md-render-buffer markdown-mode-map))

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
(elpy-enable)

(defun my-set-python-compile-command ()
  "Set python compile command."
  (set (make-local-variable 'compile-command)
       (concat "python " (file-name-base buffer-file-name) ".py")))

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

(defun dek-browse-code-python ()
  "Browse code with helm swoop (classes and functions)"
  (interactive)
  (helm-swoop :$query "\\(class[[:space:]].*\\)\\|\\(def[[:space:]].*\\)"))

(add-hook 'python-mode-hook
	  '(lambda ()
	     ;; (flycheck-mode 1)
	     (elpy-mode 1)
	     (auto-indent-mode -1)
		 (setq-local auto-indent-kill-line-at-eol nil)
             (setq-local auto-indent-on-yank-or-paste nil)
             (bind-key "RET" 'newline-and-indent python-mode-map)
             (bind-key "#" 'dek-python-crunch python-mode-map)
	     (bind-key "<f12>" 'dek-python-add-breakpoint python-mode-map)
	     (bind-key "S-<f12>" 'dek-python-find-all-breakpoints python-mode-map)
	     (bind-key "C-c t r" 'test-case-run-or-run-again python-mode-map)
	     (bind-key "C-c b" 'dek-browse-code-python python-mode-map)
	     (bind-key "C-c C-b" 'dek-browse-code-python python-mode-map)
	     (magit-dont-ignore-whitespace)
             ;; (setq paragraph-start "\\(\\s-*$\\)\\|\\(\\.$)")
             ;; (setq paragraph-start "\f\\|\\(\s-*$\\)\\|\\([-:] +.+$\\)" paragraph-seperate "$")
	     (rainbow-delimiters-mode 1)
	     ;; Do this for numpy style docstring filling
             (setq-local paragraph-separate "\\([        \f]*$\\)\\|\\(.* : .*$\\)\\|\\(.*-+$\\)")
	     (auto-complete-mode 0)
             ))

;; (use-package anaconda-mode
;;   :init
;;   (add-hook 'python-mode-hook 'anaconda-mode)
;;   (add-hook 'python-mode-hook 'my-set-python-compile-command)
;;   :config
;;   (unbind-key "M-r" anaconda-mode-map)
;;   )

;; faster imenu
(add-hook 'python-mode-hook
          (lambda ()
	    (set (make-local-variable 'imenu-create-index-function)
                 #'python-imenu-create-index)))

;; ;; jedi mode
;; ;; (setq jedi:setup-keys t)                      ; optional
;; ;; (setq jedi:complete-on-dot t)                 ; optional
;; (add-hook 'python-mode-hook 'jedi:setup)


;; ;; Use anaconda if available
;; (if (file-exists-p "~/anaconda/bin/ipython")
;;     (setq
;;      python-shell-interpreter "~/anaconda/bin/ipython"
;;      ;; python-shell-interpreter-args ""
;;      ;; python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;      ;; python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;      ;; python-shell-completion-setup-code
;;      ;; "from IPython.core.completerlib import module_completion"
;;      ;; python-shell-completion-module-string-code
;;      ;; "';'.join(module_completion('''%s'''))\n"
;;      ;; python-shell-completion-string-code
;;      ;; "';'.join(get_ipython().Completer.all_completions('''%s'''))"
;;      test-case-python-executable "~/anaconda/bin/python"

;;      ; from https://github.com/gabrielelanaro/emacs-for-python/blob/master/epy-python.el
;;      python-shell-interpreter "ipython"
;;      python-shell-interpreter-args ""
;;      python-shell-prompt-regexp "In \[[0-9]+\]: "
;;      python-shell-prompt-output-regexp "Out\[[0-9]+\]: "
;;      python-shell-completion-setup-code ""
;;      python-shell-completion-string-code "';'.join(get_ipython().complete('''%s''')[1])\n"
;;      ))

;;;;;;;;;;;;;;;;;;;;;;;;;; CYTHON MODE ;;;;;;;;;;;;;;;;;;;;;
(use-package cython-mode)
(defun dek-cython-compile ()
  (interactive)
  (let (current-dir)
    (setq currect-dir (file-name-directory (buffer-file-name)))
    (cd (projectile-project-root))
    (compile (concat (replace-regexp-in-string "ipython" "python" python-shell-interpreter)
		     " "
		     (expand-file-name "setup.py" (projectile-project-root))
		     " build_ext --inplace"))
    (cd current-dir)))

(defun dek-cython-std-compile ()
  (interactive)
  (compile
   (format cython-default-compile-format
	   (shell-quote-argument buffer-file-name))))

(add-hook 'cython-mode-hook
          '(lambda ()
             (define-key cython-mode-map (kbd "C-c C-s") 'dek-cython-compile)
	     (define-key cython-mode-map (kbd "C-c C-c") 'dek-cython-std-compile)
	     (rainbow-delimiters-mode)
             ))

(require 'dek-edit-python-docstring)

;; JINJA2
(use-package jinja2-mode
  :commands jinja2-mode
  :mode ("\\.jinja2$" . jinja2-mode))

;;;;;;;;;;; LATEX AND AUCTEX  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-to-list 'auto-mode-alist '("\\.rwthtex$" . TeX-latex-mode))
;; (add-to-list 'auto-mode-alist '("\\.tex\\'" . TeX-latex-mode))
;; (add-to-list 'auto-mode-alist '("\\.sty\\'" . TeX-latex-mode))
;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)
;; (setq-default TeX-master nil)
;; (setq preview-auto-cache-preamble t)
;; (setq reftex-plug-into-AUCTeX t)

(use-package auctex
  :commands (LaTeX-mode TeX-latex-mode)
  :config
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
               (rainbow-delimiters-mode 1)
               ))

  ;; (add-hook 'TeX-mode-hook
  ;;       '(lambda ()
  ;;    (define-key TeX-mode-map (kbd "\C-c\C-c")
  ;;      (lambda ()
  ;;    (interactive)
  ;;    (save-buffer)
  ;;    (TeX-command-menu "LaTeX")))
  ;;    (define-key TeX-mode-map (kbd "<f12>")
  ;;      (lambda ()
  ;;    (interactive)
  ;;    (TeX-view)
  ;;    [return]))))

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

  )


;(autoload 'whizzytex-mode "whizzytex"
;"WhizzyTeX, a minor-mode WYSIWIG environment for LaTeX" t)
;(setq-default whizzy-viewers '(("-pdf" "evince %s" )("-dvi" "evince %s")("-ps" "gv") ))

;;;;;;;;;;;;;;;;;;;;; PROGRAMMING MODES ;;;;;;;;;;;;;;;
;; (add-hook 'after-init-hook 'global-flycheck-mode)
;; (setq flycheck-flake8rc (expand-file-name ".flake8rc" user-emacs-directory))
;; (setq prog-mode-hooks
;;       '(matlab-mode
;; 	python-mode
;; 	latex-mode
;; 	TeX-latex-mode
;; 	emacs-lisp-mode))

;; ;; add prog-mode-hook to all programming language modes
;; (dolist (tmp-prog-mode-hook prog-mode-hooks nil)
;;   (add-hook tmp-prog-mode-hook (lambda () (run-hooks 'prog-mode-hook))))

;;;;;;;;;;;;;;;;;;;;;;;; FLYSPELL ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; DICTIONARY  AND SPELL CHECKING ;;;;;;;;;;;;;;;;;
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
	    (rainbow-delimiters-mode 1)
	    (set-face-attribute 'flyspell-incorrect nil :foreground "#ac736f" :weight 'bold)
	    (set-face-attribute 'flyspell-duplicate nil :foreground "#8c836f" :underline t)))
(add-hook 'prog-mode-hook 'hs-minor-mode)
(bind-key "C-c h" 'hs-toggle-hiding prog-mode-map)
(bind-key "C-c <f7>" 'hs-hide-all prog-mode-map)
(bind-key "C-c <f8>" 'hs-show-all prog-mode-map)

(defun dek-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
     (change (if (string= dic "german") "english" "german")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)
    ))
(bind-key "<f8>" 'dek-switch-dictionary)


;;;;;;;;;;;;;;; C AND C++ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-turn-on-auto-newline ()
  (c-toggle-auto-newline 1))
(add-hook 'c-mode-common-hook 'my-turn-on-auto-newline)

(setq c-default-style "linux")
(setq-default c-basic-offset 4)
(setq c-indent-level 4)
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)


(add-hook 'c++-mode-hook
      (lambda ()
	(unless (or (file-exists-p "makefile")
		    (file-exists-p "Makefile"))
	  (set (make-local-variable 'compile-command)
	       (concat "make -k "(file-name-sans-extension buffer-file-name))))))
(add-hook 'c++-mode-hook
		  '(lambda ()
             (setq c-default-style "linux")
             (setq c-basic-offset 4)
             (setq c-indent-level 4)
             (setq indent-tabs-mode nil)
			 (auto-indent-mode -1)
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


;;;;;;;;;;;;;;;;;;; JAVA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;; FORTRAN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-to-list 'auto-mode-alist '("\\.f\\'" . fortran-mode))
(defun dek-browse-code-fortran ()
  "This browses code subroutine and call statements."
  (interactive)
  (helm-swoop :$query "\\(SUBROUTINE[[:space:]]+\\)\\|\\(CALL[[:space:]]+\\)"))

(defun dek-fortran-hook ()
  "This is the fortran mode hook for binding keys."
  (define-key fortran-mode-map (kbd "C-c b") 'dek-browse-code-fortran)
  )

(add-hook 'fortran-mode 'dek-fortran-hook)


;;;;;;;;;;;;;;;;;;; EMACS LISP ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;; clojure mode ;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package clojure-mode
  :commands clojure-mode
  :config
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (use-package cider
    :config
    (defun my/cider-mode-hooks ()
      "Clojure specific setup code that should only be run when we
  have a CIDER REPL connection"
      (cider-turn-on-eldoc-mode))

    (add-hook 'cider-mode-hook
              'my/cider-mode-hooks)

    (defun my/cider-repl-mode-hooks ()
      (my/turn-on 'paredit
                  'rainbow-delimiters
                  'highlight-parentheses))

    (add-hook 'cider-repl-mode-hook
              'my/cider-repl-mode-hooks)

    )
  )

;;;;;;;;;;;; OTHER MODES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;; haskell-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

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

;;;;;;;;;;;;;;;;;;;;;;;;; MODELICA ;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (expand-file-name "site-lisp/modelica" user-emacs-directory))
(autoload 'modelica-mode "modelica-mode" "Modelica Editing Mode" t)
(setq auto-mode-alist (cons '("\.mop?$" . modelica-mode) auto-mode-alist))


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

(defun dek-matlab-switch-to-shell ()
  "Switch to inferior Python process buffer."
  (interactive)
  (if (get-buffer "*MATLAB*")
      (pop-to-buffer "*MATLAB*" t)
    (matlab-shell)))

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

(defun dek-clear-all-matlab ()
  "Browse code with helm swoop (classes and functions)"
  (interactive)
  (matlab-shell-send-string "close all\nclear all\nclear classes\n")
  )

(defun dek-browse-code-matlab ()
  "Browse code with helm swoop (classes and functions)"
  (interactive)
  ;; (helm-swoop :$query "\\(function[[:space:]]+[^=]*=[[:space:]]*\\)\\|\\(classdef[[:space:]]+\\)")
  (helm-imenu))


(add-hook 'matlab-shell-mode-hook
      '(lambda ()
	 (define-key matlab-shell-mode-map (kbd "<f5>") 'dek-matlab-send-dbcont)
	 (define-key matlab-shell-mode-map (kbd "<f11>") 'dek-matlab-send-dbstep)
         (define-key matlab-shell-mode-map (kbd "C-l") 'erase-buffer)
         (define-key matlab-shell-mode-map (kbd "C-c <tab>") 'dek-matlab-goto-error-line)
	 (define-key matlab-shell-mode-map (kbd "<f6>") 'matlab-shell-close-figures)
	 (define-key matlab-shell-mode-map (kbd "<f7>") 'dek-clear-all-matlab)
	 (setq-local comint-input-ring-file-name "~/.matlab/R2014a/history.m")
	 ))


(add-hook 'matlab-mode-hook
	  '(lambda ()
	     (require 'matlab-expansions)
	     ;; (auto-complete-mode 1)
	     (define-key matlab-mode-map (kbd "<f12>") 'dek-matlab-set-breakpoint)
	     (key-chord-define matlab-mode-map ";;"  "\C-e;")
	     (setq matlab-imenu-generic-expression
                   '((nil "^\\s-*\\(function *.*\\)" 1)
                     (nil "^\\s-*\\(classdef *.*\\)" 1)))
	     (define-key matlab-mode-map (kbd "<f6>") 'matlab-shell-close-figures)
	     (define-key matlab-mode-map "\C-c\C-z" 'dek-matlab-switch-to-shell)
	     (define-key matlab-mode-map (kbd "C-c b") 'dek-browse-code-matlab)
	     (define-key matlab-mode-map (kbd "<f7>") 'dek-clear-all-matlab)
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
;; (message "MATLAB ALL LOADED!!!")


;;;;;;;;;;;; PRETTY SYMBOLS / UNICODE SYMBOLS ;;;;;;;;;;;;;;
(use-package pretty-symbols
  :config
  (add-hook 'matlab-mode-hook 'pretty-symbols-mode)
  (add-hook 'python-mode-hook 'pretty-symbols-mode)
  (add-hook 'emacs-lisp-mode-hook 'pretty-symbols-mode))


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

(use-package virtualenv)

;; ;;;;;;;;;;;;;;;;;;;;;;; XIKI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Load el4r, which loads Xiki
;; (add-to-list 'load-path "~/.gem/ruby/2.1.0/gems/trogdoro-el4r-1.0.10/data/emacs/site-lisp/")
;; (require 'el4r)
;; (el4r-boot)
;; (el4r-troubleshooting-keys)

;; '(LaTeX-command "latex")
;; '(LaTeX-command-style (quote (("" "%(PDF)%(latex) -shell-escape %S%(PDFout)"))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command-style
   (quote
    (("" "%(PDF)%(latex) -shell-escape %(extraopts) %S%(PDFout)"))))
 '(ansi-term-color-vector
   [unspecified "#282a2e" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#81a2be" "#e0e0e0"] t)
 '(auto-indent-on-visit-pretend-nothing-changed nil)
 '(custom-safe-themes
   (quote
    ("1cf3f29294c5a3509b7eb3ff9e96f8e8db9d2d08322620a04d862e40dc201fe2" "cd70962b469931807533f5ab78293e901253f5eeb133a46c2965359f23bfb2ea" "769bb56fb9fd7e73459dcdbbfbae1f13e734cdde3cf82f06a067439568cdaa95" "253bd40645913cc95b9f8ef0533082cb9a4cb0810f854c030f3ef833ee5b9731" "1f31a5f247d0524ef9c051d45f72bae6045b4187ed7578a7b1f8cb8758f92b60" default)))
 '(dired-dwim-target t)
 '(elpy-mode-hook nil)
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-sane-defaults)))
 '(elpy-rpc-python-command "~/anaconda/bin/python")
 '(elpy-test-runner (quote elpy-test-pytest-runner))
 '(fci-rule-color "#2b2b2b")
 '(fill-column 79)
 '(flycheck-check-syntax-automatically (quote (save new-line mode-enabled)))
 '(flycheck-idle-change-delay 2)
 '(flymake-no-changes-timeout 1.5)
 '(fortran-do-indent 2)
 '(fortran-if-indent 2)
 '(global-semantic-decoration-mode t)
 '(global-semantic-highlight-func-mode t)
 '(global-semantic-stickyfunc-mode nil)
 '(jedi:key-complete [backtab])
 '(magit-diff-options (quote ("--ignore-space-change")))
 '(matlab-case-level (quote (4 . 4)))
 '(matlab-fill-code nil)
 '(matlab-shell-command-switches (quote ("-nodesktop" "-nosplash")) t)
 '(minimap-dedicated-window t)
 '(minimap-display-semantic-overlays t)
 '(minimap-update-delay 0.25)
 '(minimap-width-fraction 0.1)
 '(org-babel-python-command "python")
 '(org-confirm-babel-evaluate nil)
 '(org-export-babel-evaluate t)
 '(org-export-backends (quote (ascii html icalendar latex md odt)))
 '(pretty-symbol-categories (lambda relational))
 '(pretty-symbol-patterns
   (quote
    ((8230 lambda "\\.\\.\\."
           (matlab-mode))
     (402 lambda "\\<function\\>"
          (js-mode))
     (8800 relational "!="
           (c-mode c++-mode go-mode java-mode js-mode perl-mode cperl-mode ruby-mode python-mode inferior-python-mode))
     (8800 relational "~="
           (matlab-mode))
     (8800 relational "/="
           (emacs-lisp-mode inferior-lisp-mode inferior-emacs-lisp-mode lisp-mode scheme-mode))
     (8805 relational ">="
           (c-mode c++-mode go-mode java-mode js-mode perl-mode cperl-mode ruby-mode python-mode inferior-python-mode emacs-lisp-mode inferior-lisp-mode inferior-emacs-lisp-mode lisp-mode scheme-mode matlab-mode))
     (8804 relational "<="
           (c-mode c++-mode go-mode java-mode js-mode perl-mode cperl-mode ruby-mode python-mode inferior-python-mode emacs-lisp-mode inferior-lisp-mode inferior-emacs-lisp-mode lisp-mode scheme-mode matlab-mode))
     (8743 logical "&&"
           (c-mode c++-mode go-mode java-mode js-mode perl-mode cperl-mode ruby-mode python-mode inferior-python-mode))
     (8743 logical "\\<and\\>"
           (emacs-lisp-mode inferior-lisp-mode inferior-emacs-lisp-mode lisp-mode scheme-mode))
     (8744 logical "||"
           (c-mode c++-mode go-mode java-mode js-mode perl-mode cperl-mode ruby-mode python-mode inferior-python-mode matlab-mode))
     (8744 logical "\\<or\\>"
           (emacs-lisp-mode inferior-lisp-mode inferior-emacs-lisp-mode lisp-mode scheme-mode))
     (8709 nil "\\<nil\\>"
           (emacs-lisp-mode inferior-lisp-mode inferior-emacs-lisp-mode lisp-mode scheme-mode)))))
 '(pyvenv-virtualenvwrapper-python "~/anaconda/bin/python")
 '(reftex-ref-style-alist
   (quote
    (("Default" t
      (("\\cref" 13)
       ("\\cpageref" 112)))
     ("Varioref" "varioref"
      (("\\vref" 118)
       ("\\vpageref" 103)
       ("\\Vref" 86)
       ("\\Ref" 82)))
     ("Fancyref" "fancyref"
      (("\\fref" 102)
       ("\\Fref" 70)))
     ("Hyperref" "hyperref"
      (("\\autoref" 97)
       ("\\autopageref" 117))))))
 '(reftex-ref-style-default-list (quote ("Default")))
 '(rst-indent-field 4)
 '(rst-indent-width 4)
 '(safe-local-variable-values (quote ((TeX-master . "thesis-master") (TeX-master . t))))
 '(semantic-default-submodes
   (quote
    (global-semantic-decoration-mode global-semantic-stickyfunc-mode global-semantic-idle-scheduler-mode global-semanticdb-minor-mode)))
 '(semantic-mode t)
 '(switch-window-shortcut-style (quote qwerty))
 '(test-case-python-executable "~/anaconda/bin/python")
 '(virtualenv-root "~/anaconda/envs/")
 '(warning-suppress-types (quote ((undo discard-info)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monaco"))))
 '(flymake-errline ((t (:background "#383131" :underline nil))))
 '(flymake-warnline ((t (:background "#366060" :underline nil))))
 '(fringe ((t (:background "#4f4f4f" :foreground "#dcdccc" :weight normal :height 0.3 :width condensed))))
 '(ivy-current-match ((t (:inherit default :background "dim gray" :weight bold))))
 '(minimap-font-face ((t (:height 30 :family "DejaVu Sans Mono"))))
 '(mode-line ((t (:background "#506070" :foreground "#dcdccc" :box (:line-width -1 :style released-button) :family "Ubuntu Condensed"))))
 '(mode-line-inactive ((t (:background "#555555" :foreground "#808080" :box nil :family "Ubuntu Condensed"))))
 '(semantic-tag-boundary-face ((t (:overline "SeaGreen4"))))
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
  ;; (concat "echo " "'" (file-name-directory (buffer-file-name)) "' > ~/.ld" )
  (concat "echo " "'" default-directory "' > ~/.ld" )
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
(put 'downcase-region 'disabled nil)

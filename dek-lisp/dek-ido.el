;;; dek-ido.el ---

;; Copyright (C) Davis Kirkendall
;;
;; Status: not intended to be distributed yet



(ido-mode 1)
;(ido-everywhere 1)
(setq
 ido-enable-tramp-completion t
 ido-enable-flex-matching 1
 ido-enable-last-directory-history 1
 ;setq ido-show-dot-for-dired nil ;; put . as the first item
 ;setq ido-use-filename-at-point t ;; prefer file names near point
 )


;; disable auto searching for files unless called explicitly
(setq ido-auto-merge-delay-time 99999)
(define-key ido-file-dir-completion-map (kbd "C-c C-s")
  (lambda()
    (interactive)
    (ido-initiate-auto-merge (current-buffer))))

(defun dek-ido-switch-buffer ()
  "thisandthat."
  (interactive)
  (let (ido-ignore-buffers)
    (setq ido-ignore-buffers '("^\\s-" "^\\*" "TAGS$"))
    (ido-switch-buffer)
    )
  )

(defun dek-ido-trigger-show-switch-buffer ()
  "thisandthat."
  (interactive)
  (if (eq ido-common-match-string nil)
      (ido-exit-minibuffer)
    (ido-complete))
  )

(defun my-horizontal-smex ()
  "thisandthat."
  (interactive)
  (let ((ido-decorations (quote ("-> " "" " | " " ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))))
    (smex))
  )

(defun dek-init-smex ()
  (smex-initialize)
  (global-set-key (kbd "M-x") 'my-horizontal-smex))

  
(if (equal user-login-name "dek")    
    (dek-init-smex))

(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
  (defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

;;; dek-ido.el ends here

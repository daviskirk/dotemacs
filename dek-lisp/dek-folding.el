;;{{{ ;;;;;;;;;;; FOLDING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'folding-mode          "folding" "Folding mode" t)
(autoload 'turn-off-folding-mode "folding" "Folding mode" t)
(autoload 'turn-on-folding-mode  "folding" "Folding mode" t)

;; ;; (setq folding-default-keys-function
;; ;;      'folding-bind-backward-compatible-keys)

(defun dek-folding-load-hook ()
  "Deks folding mode load hook"
  (folding-install)  ;; just to be sure
  (turn-on-folding-mode)
  (setq folding-default-keys-function
	'folding-bind-outline-compatible-keys)
  (defvar folding-mode-marks-alist nil)
  (folding-add-to-marks-list 'latex-mode "% {{{"  "% }}}"  nil nil)
  (folding-add-to-marks-list 'LaTeX-mode "% {{{"  "% }}}"  nil nil)
  (folding-add-to-marks-list 'lua-mode "-- {{{"  "-- }}}"  nil nil)
  (folding-add-to-marks-list 'conf-mode "# {{{"  "# }}}"  nil nil)
  (folding-add-to-marks-list 'conf-space-mode "# {{{"  "# }}}"  nil nil)
  (folding-add-to-marks-list 'haskell-mode "-- {{{"  "-- }}}"  nil nil)

  (define-key folding-mode-map (kbd "C-8") 'folding-toggle-show-hide)
  (define-key folding-mode-map (kbd "C-c @ C-q") 'folding-toggle-show-hide)
  )

(setq folding-load-hook 'dek-folding-load-hook)


;; (if (load "folding" 'nomessage 'noerror)
;;     (folding-mode-add-find-file-hook))


;; (load "folding" 'nomessage 'noerror)

(setq folding-modes '(LaTeX-mode-hook
		      lua-mode-hook
		      conf-mode-hook
		      conf-space-mode-hook
		      haskell-mode-hook
		      emacs-lisp-mode-hook
		      lisp-mode))

(add-hook 'emacs-lisp-mode-hook '(lambda () (folding-mode 1)))
(add-hook 'lua-mode-hook '(lambda () (folding-mode 1)))

(loop for tmp-mode-hook in folding-modes do
      (add-hook tmp-mode-hook '(lambda () (folding-mode 1))))

;; ü for Überschriften
;; (global-set-key (kbd "C-ü C-ü") 'folding-toggle-show-hide)
;; (global-set-key (kbd "C-ü C-SPC") 'folding-next-visible-heading)
;; (global-set-key (kbd "C-ü r") 'folding-fold-region)(transient-mark-mode 1)
;; (global-set-key (kbd "C-ü a") 'folding-show-all)
;; (setq folding-mode-prefix-key (kbd "C-c @"))

(defun dek-folding-box (folding-block-name)
  "Make a Box for a folding block"
  (interactive "sFolding Block Name: ")
  (let (name-length
	comment-length-theoretical
	comment-length-before
	comment-length-after
	start-mark
	end-mark)
    (setq name-length (length folding-block-name))
    (setq start-mark (nth 0 (folding-get-mode-marks)))
    (setq end-mark (nth 1 (folding-get-mode-marks)))
    ;; comment-length - start mark length - 1 for the space after start-mark
    ;; - 2 for space before and after name - name-length
    (setq comment-length-theoretical (- 60 (length start-mark) 1 2 name-length))
    (if (equal (mod comment-length-theoretical 2) 0)
	(setq comment-length-before (/ comment-length-theoretical 2)
	      comment-length-after (/ comment-length-theoretical 2))
      (setq comment-length-before (/ (- comment-length-theoretical 1) 2)
	    comment-length-after (/ (+ comment-length-theoretical 1) 2)))

    (insert comment-start
	    (make-string (- 60 (length comment-start)) (string-to-char comment-start))
	    "\n" start-mark " "
	    (make-string comment-length-before (string-to-char comment-start))
	    " " folding-block-name " "
	    (make-string comment-length-after (string-to-char comment-start))
	    "\n\n"
	    end-mark "\n"
	    comment-start
	    (make-string (- 60 (length comment-start)) (string-to-char comment-start)))
    (line-move -2)
    ))

;;; dek-edit-python-docstring --- package for editing python docstrings
;;
;;; Commentary:
;;
;; This package contains a few hacks for editing python docstrings
;;
;;; Code:

(defvar dek-python-docstring-buffer "*python-docstring*")
(defvar dek-python-source-buffer nil)
(defvar dek-python-source-point nil)

(defun dek-python-insert-docstring ()
  "Insert docstring back into original buffer.

Insert docstring saved in DEK-PYTHON-DOCSTRING-BUFFER into the
docstring present in DEK-PYTHON-SOURCE-BUFFER at point
DEK-PYTHON-SOURCE-POINT."
  (interactive)
  (pop-to-buffer dek-python-source-buffer)
  ;; (with-current-buffer dek-python-source-buffer
    (goto-char dek-python-source-point)
    (let (startp endp docstring-point raw-docstring docstring more-lines (indent (current-indentation)) begl endl)
      (goto-char (nth 8 (syntax-ppss)))
      (setq startp (+ (point) 3))
      (forward-sexp 1)
      (backward-char 3)
      (setq endp (point))
      (delete-region startp endp)
      (with-current-buffer dek-python-docstring-buffer
      	(setq docstring-point (point))
      	(setq raw-docstring (buffer-string))
      	(with-temp-buffer
      	  (insert raw-docstring)
	  (goto-char 1)
      	  (setq more-lines (= (forward-line) 0))
      	  (while more-lines
      	    (beginning-of-line)
	    (insert-char (string-to-char " ") indent)
      	    (setq more-lines (= (forward-line) 0)))
      	  (setq docstring (buffer-string))))
      (insert docstring)
      (goto-char (+ startp docstring-point))
      )
    (pop-to-buffer dek-python-docstring-buffer)
    ;; )
    )

(defun dek-python-edit-docstring ()
  "Edit python docstring.

Copy and current docstring to DEK-PYTHON-DOCSTRING-BUFFER for editing in `rst-mode'."
  (interactive)
  (let (prep postp indent)
    (setq dek-python-source-point (point))
    (save-excursion
      (setq prep (nth 8 (syntax-ppss)))
        (goto-char prep)
        (forward-sexp 1)
        (copy-to-buffer dek-python-docstring-buffer (+ prep 3) (- (point) 3))
        (goto-char prep)
        (beginning-of-line)
	(setq indent (current-indentation))
        (with-current-buffer dek-python-docstring-buffer
          (let (begl endl diff (more-lines t) (docstring-point (- dek-python-source-point (+ prep 3))))
	    (goto-char 1)
            (setq more-lines (= (forward-line) 0))
            (while more-lines
              (beginning-of-line)
              (setq begl (point))
              (end-of-line)
              (setq endl (point))
              (if (> indent (- endl begl))
		  nil
		(delete-region begl (+ begl indent))
		(if (< (point) docstring-point)
		    (setq docstring-point (- docstring-point indent))))
              (setq more-lines (= (forward-line) 0))
              )
	    (goto-char docstring-point)
	    )
	  )
	)
    (setq dek-python-source-buffer (current-buffer))
    (setq dek-python-source-point (point))
    (switch-to-buffer-other-window dek-python-docstring-buffer)
    (rst-mode)
    (local-set-key "\C-c\C-c" 'dek-python-insert-docstring)
    (local-set-key "\C-x\C-s" 'dek-python-insert-docstring)
    (setq-local paragraph-separate "\\([        \f]*$\\)\\|\\(.* : .*$\\)\\|\\(.*-+$\\)")
    ))

(provide 'dek-edit-python-docstring)
;;; dek-edit-python-docstring.el ends here

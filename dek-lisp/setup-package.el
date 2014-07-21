(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
	     ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(unless (and (file-exists-p "~/.emacs.d/elpa/archives/gnu")
	     (file-exists-p "~/.emacs.d/elpa/archives/melpa"))
  (package-refresh-contents))

(defvar dek-used-packages
  '(
    clojure-mode
    clojure-snippets
    auctex
    auto-complete
    autopair
    color-theme
    csv-mode
    csv-nav
    dired+
    dired-details+
    dired-details
    dired-single
    edit-server
    fill-column-indicator
    button-lock
    flycheck
    dash
    flymake-cursor
    frame-cmds
    frame-fns
    haskell-mode
    helm
    helm-c-yasnippet
    helm-git
    helm-swoop
    highlight-parentheses
    iy-go-to-char
    key-chord
    lua-mode
    magit
    multiple-cursors
    nav-flash
    org-bullets
    org-cua-dwim
    php-mode
    powerline
    guru-mode
    rainbow-delimiters
    slime
    smart-operator
    smartrep
    smex
    smooth-scrolling
    virtualenv
    web-mode
    websocket
    yaml-mode
    yasnippet
    zenburn-theme
    zoom-frm
    helm-themes
    projectile
    auto-indent-mode
    org
    ))

(defun dek-packages-install (packages)
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
	   (package-install package))))
 packages))

(dek-packages-install dek-used-packages)

(provide 'setup-package)

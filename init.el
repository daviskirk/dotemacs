(require 'org)
(org-babel-load-file
 (expand-file-name "dek-emacs.org" user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command-style
   (quote
    (("" "%(PDF)%(latex) -shell-escape %(extraopts) %S%(PDFout)"))))
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(ansi-term-color-vector
   [unspecified "#282a2e" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#81a2be" "#e0e0e0"] t)
 '(auto-indent-on-visit-pretend-nothing-changed nil)
 '(custom-safe-themes
   (quote
    ("f4deb2b3c1a11ed40a4a6a8d3d3fe5a024fb81488452ac3b34aa6fc56bbcc69a" "6c2f1685374aeff638a923f1ec5408d5db20c83ab1b3e137600c8501ec7df5ee" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" "1cf3f29294c5a3509b7eb3ff9e96f8e8db9d2d08322620a04d862e40dc201fe2" "cd70962b469931807533f5ab78293e901253f5eeb133a46c2965359f23bfb2ea" "769bb56fb9fd7e73459dcdbbfbae1f13e734cdde3cf82f06a067439568cdaa95" "253bd40645913cc95b9f8ef0533082cb9a4cb0810f854c030f3ef833ee5b9731" "1f31a5f247d0524ef9c051d45f72bae6045b4187ed7578a7b1f8cb8758f92b60" default)))
 '(dired-dwim-target t)
 '(electric-operator-enable-in-docs t)
 '(elpy-mode-hook nil)
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-sane-defaults)))
 '(elpy-rpc-python-command "~/anaconda/envs/emacs/bin/python")
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
 '(ivy-height 15)
 '(ivy-mode t)
 '(ivy-wrap t)
 '(jedi:key-complete [backtab])
 '(magit-diff-arguments (quote ("--stat" "--no-ext-diff" "--ignore-space-change")))
 '(magit-diff-options (quote ("--ignore-space-change")))
 '(markdown-command "marked")
 '(markdown-link-space-sub-char "-")
 '(matlab-case-level (quote (4 . 4)))
 '(matlab-fill-code nil)
 '(matlab-shell-command-switches (quote ("-nodesktop" "-nosplash")) t)
 '(minimap-dedicated-window t)
 '(minimap-display-semantic-overlays t)
 '(minimap-update-delay 0.25)
 '(minimap-width-fraction 0.1)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-babel-python-command "python")
 '(org-confirm-babel-evaluate nil)
 '(org-export-babel-evaluate t)
 '(org-export-backends (quote (ascii html icalendar latex md odt)))
 '(org-plantuml-jar-path "/opt/plantuml/plantuml.jar")
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
 '(puml-plantuml-jar-path "/opt/plantuml/plantuml.jar")
 '(py-indent-honors-multiline-listing t)
 '(py-lhs-inbound-indent 0)
 '(python-check-command "~/anaconda/envs/emacs/bin/flake8")
 '(python-shell-exec-path (quote ("~/anaconda/envs/emacs/bin")))
 '(python-shell-interpreter "~/anaconda/envs/emacs/bin/ipython")
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
 '(smex-completion-method (quote ivy))
 '(smex-save-file "~/.emacs.d/.smex-items")
 '(switch-window-shortcut-style (quote qwerty))
 '(test-case-python-executable "~/anaconda/bin/python")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(virtualenv-root "~/anaconda/envs/")
 '(warning-suppress-types (quote ((undo discard-info)))))

(message "Config Loaded!")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-template-field ((t (:background "dark sea green" :foreground "black")))))

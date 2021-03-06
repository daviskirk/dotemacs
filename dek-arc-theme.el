(deftheme dek-arc
  "Created 2015-12-11.")

(custom-theme-set-faces
 'dek-arc
 '(default ((t (:inherit nil :stipple nil :background "#31323A" :foreground "#DCDCCC" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "unknown" :family "Monaco"))))
 '(cursor ((t (:foreground "#DCDCCC" :background "#FFFFEF"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((t (:family "Sans Serif"))))
 '(escape-glyph ((t (:weight bold :foreground "#F0DFAF"))))
 '(minibuffer-prompt ((t (:foreground "#F0DFAF"))))
 '(highlight ((t (:background "#36393E"))))
 '(region ((t (:background "#1B2B35"))))
 '(shadow ((t (:foreground "grey70"))))
 '(secondary-selection ((t (:background "#4F5F5F"))))
 '(trailing-whitespace ((t (:background "#CC9393"))))
 '(flymake-errline ((t (:background "#1B2B35" :underline nil))))
 '(font-lock-builtin-face ((t (:weight bold :foreground "#DCDCCC"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#5F7F5F"))))
 '(font-lock-comment-face ((t (:foreground "#7F9F7F"))))
 '(font-lock-constant-face ((t (:foreground "#BFEBBF"))))
 '(font-lock-doc-face ((t (:foreground "#9FC59F"))))
 '(font-lock-function-name-face ((t (:foreground "#93E0F3"))))
 '(font-lock-keyword-face ((t (:weight bold :foreground "#F0DFAF"))))
 '(font-lock-negation-char-face ((t (:weight bold :foreground "#F0DFAF"))))
 '(font-lock-preprocessor-face ((t (:foreground "#64BFF3"))))
 '(font-lock-regexp-grouping-backslash ((t (:weight bold :foreground "#7F9F7F"))))
 '(font-lock-regexp-grouping-construct ((t (:weight bold :foreground "#F0DFAF"))))
 '(font-lock-string-face ((t (:foreground "#CC9393"))))
 '(font-lock-type-face ((t (:foreground "#7CB8DB"))))
 '(font-lock-variable-name-face ((t (:foreground "#DFAF8F"))))
 '(font-lock-warning-face ((t (:weight bold :foreground "#D0BF8F"))))
 '(button ((t (:underline (:color foreground-color :style line)))))
 '(link ((t (:weight bold :underline (:color foreground-color :style line) :foreground "#F0DFAF"))))
 '(link-visited ((t (:weight normal :underline (:color foreground-color :style line) :foreground "#D0BF8F"))))
 '(header-line ((t (:box (:line-width -1 :color nil :style released-button) :foreground "#F0DFAF" :background "#2B2B2B"))))
 '(tooltip ((t (:foreground "#DCDCCC" :background "#4F4F4F"))))
 '(mode-line ((t (:family "Ubuntu Condensed" :box (:line-width -1 :color nil :style released-button) :foreground "#dcdccc" :background "#606060"))))
 '(mode-line-buffer-id ((t (:weight bold :foreground "#F0DFAF"))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((t (:family "Ubuntu Condensed" :box nil :foreground "#808080" :background "#555555"))))
 '(isearch ((t (:weight bold :foreground "#D0BF8F" :background "#5F5F5F"))))
 '(isearch-fail ((t (:foreground "#DCDCCC" :background "#8C5353"))))
 '(lazy-highlight ((t (:weight bold :foreground "#D0BF8F" :background "#383838"))))
 '(match ((t (:weight bold :foreground "#DFAF8F" :background "#2B2B2B"))))
 '(next-error ((t (:inherit (region)))))
 ;; old zenburn stuff
 '(avy-background-face ((t (:foreground "#656555" :inverse-video nil))))
 '(avy-lead-face ((t (:background "#0F0F0F" :foreground "#9FF59F" :inverse-video nil))))
 '(flymake-errline ((t (:background "#383131" :underline nil))))
 '(flymake-warnline ((t (:background "#366060" :underline nil))))
 '(ivy-current-match ((t (:inherit default :background "dim gray" :weight bold))))
 '(minimap-font-face ((t (:height 30 :family "DejaVu Sans Mono"))))
 '(mode-line ((t (:background "#506070" :foreground "#dcdccc" :box (:line-width -1 :style released-button) :family "Ubuntu Condensed"))))
 '(mode-line-inactive ((t (:background "#353555" :foreground "#808080" :box nil :family "Ubuntu Condensed"))))
 '(semantic-tag-boundary-face ((t (:overline "SeaGreen4"))))
 '(sp-show-pair-match-face ((t (:background "#1F1F2F" :weight bold))))
 '(helm-ff-file ((t (:background "#31323A"))))
 '(helm-buffer-not-saved ((t (:background "#31323A"))))
 '(helm-buffer-process ((t (:background "#31323A"))))
 '(helm-buffer-saved-out ((t (:background "#31323A"))))
 '(helm-buffer-size ((t (:background "#31323A"))))
 '(fringe ((t (:width condensed :height 0.3 :weight normal :foreground "#dcdccc" :background "#3e444F"))))
 '(ivy-minibuffer-match-face-1 ((t (:background "#31323A"))))
 '(ivy-subdir ((t (:background "#31323A"))))
 )

(provide-theme 'dek-arc)

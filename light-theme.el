(deftheme light
  "Sane light theme")

(custom-theme-set-variables
 'light
 '(initial-frame-alist (quote ((fullscreen . maximized))))

;; TODO: why repeating this here and in init.file?
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("melpa-stable" . "http://stable.melpa.org/packages/"))))

;; TODO: Do I need any of these packages?
 '(package-selected-packages (quote (company-lsp company lsp-mode go-mode haskell-mode)))
 '(lsp-enable-snippet nil)

 ;; highlight the line containing point
 '(global-hl-line-mode t)

 ;; delete selected region when typing. By default emacs only inserts a new word
 '(delete-selection-mode t)

 ;; display the current column number
 '(column-number-mode t)

 ;; show line numbers
 '(global-display-line-numbers-mode t)

 ;; search everywhere
 '(apropos-do-all t)

 ;; use text mode instead of lisp mode by default
 '(initial-major-mode 'text-mode)

 ;; enable whitespace mode
 '(global-whitespace-mode t)
 ;; which kind of blank is visualized
 '(whitespace-style (quote (face spaces tabs space-mark tab-mark)))
 '(whitespace-display-mappings
     '((space-mark   ?\     [?·]     [?.])            ; space - middle dot
       (space-mark   ?\xA0  [?¤]     [?_])            ; hard space - currency sign
       (tab-mark     ?\t    [?» ?\t] [?\\ ?\t]))))    ; tab - right guillemet

(custom-theme-set-faces
 'light
 '(hl-line               ((t (:background "#fcfaed"))))
 '(region                ((t (:background "#7392f3" :foreground "#ffffff"))))
 '(whitespace-space      ((t (:background "#ffffff" :foreground "#bad3e6"))))
 '(whitespace-tab        ((t (:background "#ffffff" :foreground "#bad3e6"))))
 '(cua-rectangle         ((t (:background "#7392f3" :foreground "#ffffff")))))

(provide-theme 'light)

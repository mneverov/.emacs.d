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
 '(global-hl-line-mode 1)
 ;; show all files, not folders only
 '(speedbar-show-unknown-files t))

(custom-theme-set-faces
 'light
 '(hl-line      ((t (:background "#FCFAED"))))
 '(region       ((t (:background "#7392F3" :foreground "#ffffff")))))

(provide-theme 'light)

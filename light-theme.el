(deftheme light
  "Sane light theme")

;;(global-hl-line-mode 1)
;;(set-face-background 'hl-line "#FCFAED")
;;(set-face-foreground 'highlight nil)

(custom-theme-set-variables
 'light
 '(initial-frame-alist (quote ((fullscreen . maximized))))
;; TODO: why repeating this here and in init.file?
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("melpa-stable" . "http://stable.melpa.org/packages/"))))
;; TODO: Do I need any of these packages?
 '(package-selected-packages (quote (company-lsp company lsp-mode go-mode haskell-mode)))
 '(lsp-enable-snippet nil)
 '(global-hl-line-mode 1)
 '(speedbar 1))

(custom-theme-set-faces
 'light
 '(hl-line      ((t (:background "#FCFAED"))))
 '(region       ((t (:background "#7392F3" :foreground "#ffffff"))))
 ;; '(hl-line ((t (:inherit highlight :background "#FCFAED"))))
 )

(provide-theme 'light)

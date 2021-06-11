(deftheme mneverov
  "Created 2020-11-14.")

(custom-theme-set-variables
 'mneverov
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("melpa-stable" . "http://stable.melpa.org/packages/"))))
 '(package-selected-packages (quote (company-lsp company lsp-mode go-mode haskell-mode)))
 '(lsp-enable-snippet nil))

(provide-theme 'mneverov)

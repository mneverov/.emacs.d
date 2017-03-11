;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(add-to-list 'load-path "~/.emacs.d/lisp/")

(package-initialize)

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell.
(http://stackoverflow.com/questions/8606954/path-and-exec-path-set-but-emacs-does-not-find-executable)"
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(setq-default indent-tabs-mode nil)
(setq-default cursor-type 'bar)
(setq-default blink-cursor-blinks 0)
(show-paren-mode 1)
(setq show-paren-delay 0)
(setq multi-term-program "/bin/zsh")


(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(global-set-key (kbd "M-s-<right>") 'next-multiframe-window)
(global-set-key (kbd "M-s-<left>") 'previous-multiframe-window)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-hl-line-mode 1)
(setq-default column-number-mode 1)
(delete-selection-mode 1)

(require 'package)
(require 'multi-term)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa-stable" . "http://stable.melpa.org/packages/"))))
 '(package-selected-packages (quote (haskell-mode))))


(global-set-key (kbd "C->") (
        lambda() (interactive) (next-line) (recenter-top-bottom '(middle)))) 

(global-set-key (kbd "C-<") (
        lambda() (interactive) (previous-line) (recenter-top-bottom '(middle))))

;; By default yanking into the term doesn't work. The following allows to yank into the terminal
(add-hook 'term-mode-hook (lambda ()
                            (define-key term-raw-map (kbd "C-y") 'term-paste)))



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

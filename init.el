(require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; refresh package list if it is not already available
(when (not package-archive-contents) (package-refresh-contents))

;; install use-package if it isn't already installed
(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

;; dependencies
(setq lisp-dir
      (expand-file-name "lisp" user-emacs-directory))

(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))

;; load customizations
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path lisp-dir)

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.
   stackoverflow.com/questions/8606954/path-and-exec-path-set-but-emacs-does-not-find-executable"
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; do not show startup screen
(setq inhibit-startup-message t)
(setq-default indent-tabs-mode nil)
(setq-default cursor-type 'bar)
(setq-default blink-cursor-blinks 0)
;; show matching parenthesis
(show-paren-mode 1)
;; highlight matching parenthesis after 0 sec
(setq show-paren-delay 0)
(setq multi-term-program "/bin/zsh")
;; bind cmd to meta on mac:
(setq ns-command-modifier 'meta)
;; bind meta to cmd on mac:
(setq ns-alternate-modifier 'super)
;; enable the display of time in the modeline
(display-time-mode 1)
;; show time hh:mm dd/mm
(setq display-time-format "%H:%M %d/%m")
;; do not display system load average
(setq display-time-default-load-average nil)
;; do not show toolbar
(tool-bar-mode -1)
;; ignore case on file name completion
(setq read-file-name-completion-ignore-case 1)
;; ignore case on buffer completion
(setq read-buffer-completion-ignore-case 1)
;; calendar start day monday
(setq calendar-week-start-day 1)
;; no backups
(setq make-backup-files nil)
;; no .saves files
(setq auto-save-list-file-name nil)
;; no auto save
(setq auto-save-default nil)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(global-unset-key (kbd "M-l"))
(global-set-key (kbd "M-l") 'next-multiframe-window)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key (kbd "M-1") 'sr-speedbar-toggle)
(global-set-key (kbd "M-j") 'bs-show)
(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "M-<up>") '(lambda () (interactive) (enlarge-window 1)))
(global-set-key (kbd "M-<down>") '(lambda () (interactive) (enlarge-window -1)))
(global-set-key (kbd "M-<right>") '(lambda () (interactive) (enlarge-window-horizontally 1)))
(global-set-key (kbd "M-<left>") '(lambda () (interactive) (enlarge-window-horizontally -1)))

(global-hl-line-mode 1)
(setq-default column-number-mode 1)
(delete-selection-mode 1)

;; plugins
(require 'multi-scratch)
(require 'multi-term)

;; https://www.emacswiki.org/emacs/InteractivelyDoThings
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; buffer selection
;; https://www.emacswiki.org/emacs/BufferSelection
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/bs.el
(require 'bs)
(setq bs-configurations
      '(("files" "^\\*scratch\\*" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)))

;; speedbar https://www.emacswiki.org/emacs/SpeedBar
(require 'sr-speedbar)
;; show hidden filesl
(setq speedbar-directory-unshown-regexp "^$")

(eval-after-load 'dired '(require 'setup-dired))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (mneverov)))
 '(custom-safe-themes
   (quote
    ("e0dd51fcda6a7462f86eca3bddca85cb4472eb5fd0acb6090dff21c9a9a62a7a" default)))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(lsp-enable-snippet nil)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa-stable" . "http://stable.melpa.org/packages/"))))
 '(package-selected-packages (quote (company-lsp company lsp-mode go-mode haskell-mode))))

;; do not yank ever
(global-set-key "\C-w" 'backward-delete-word)
(global-set-key (kbd "M-d") 'delete-word)

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-word (- arg)))


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

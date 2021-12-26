(require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; refresh package list if it is not already available
(when (not package-archive-contents) (package-refresh-contents))

;; install use-package if it isn't already installed
(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

;; dependencies
(setq site-dir
      (expand-file-name "site" user-emacs-directory))

(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))

;; load customizations
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path site-dir)

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.
   stackoverflow.com/questions/8606954/path-and-exec-path-set-but-emacs-does-not-find-executable"
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(setq ring-bell-function 'ignore)
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
;; no confirmation on exit when modified buffers exist
(setq confirm-kill-emacs 'y-or-n-p)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(global-unset-key (kbd "M-l"))
(global-set-key (kbd "M-l") 'next-multiframe-window)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-k" 'delete-eol-or-region)
(global-set-key (kbd "M-1") 'sr-speedbar-toggle)
(global-set-key (kbd "M-j") 'bs-show)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "\C-h a") 'apropos)

(global-set-key (kbd "M-<up>") '(lambda () (interactive) (enlarge-window 1)))
(global-set-key (kbd "M-<down>") '(lambda () (interactive) (enlarge-window -1)))
(global-set-key (kbd "M-<right>") '(lambda () (interactive) (enlarge-window-horizontally 1)))
(global-set-key (kbd "M-<left>") '(lambda () (interactive) (enlarge-window-horizontally -1)))

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

(defun delete-eol-or-region ()
  "Deletes from the caret position until the end of the current line or
deletes the selection."
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point) (line-end-position))))

(global-set-key (kbd "C->") (
        lambda() (interactive) (next-line) (recenter-top-bottom '(middle))))

(global-set-key (kbd "C-<") (
        lambda() (interactive) (previous-line) (recenter-top-bottom '(middle))))

;; By default yanking into the term doesn't work. The following allows to yank into the terminal
(add-hook 'term-mode-hook (lambda ()
                            (define-key term-raw-map (kbd "C-y") 'term-paste)))

;; check https://github.com/emacs-mirror/emacs/blob/master/lisp/simple.el for
;; examples from the emacs predefined functions.

;; plugins
;; TODO: do I need any of these?
;;(require 'multi-scratch)
;;(require 'multi-term)

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

;; use SrSpeedbar https://www.emacswiki.org/emacs/SrSpeedbar instead of standard
;; speedbar because the latter opens in a different frame.
(require 'sr-speedbar)
;; show hidden files
(setq speedbar-directory-unshown-regexp "^$")

(eval-after-load 'dired '(require 'setup-dired))

;; this will search for and load ***-theme.el (light-theme.el) in ~/.config/emacs.
(load-theme 'light t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0eb866723f81a3a28159505da2616086722328abc7ef118bfc8778f1667964e5" "17adbcb90b1478ea5deef7c659ea4f7d9e6a120f5422fef074d95c938c088f21" "2e76c530f8939d8e269c1f3fcb1c1c4c8e4d15ebd153799b3dba4ab7ae3c1d57" "194ec31c4450ddc1d5e0490dc1eeda783ac5312542a76cdc065381e480eebbe7" "944f86bc721b184a46de9efaa81b4963d95ff77214466570bf9e757d997dd3dc" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

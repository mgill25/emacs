;; First require Marmalade repo
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)

;; Install starter packages if not there already.
;; Starter package link: https://github.com/technomancy/emacs-starter-kit
(setq package-enable-at-startup nil)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/packages/")
;; OS X: Pseudo Daemon mode: Create a new frame every time
;; we exit from an old one, for emacsclient, so OS X doc icon doesn't become
;; useless.
; (require 'osx-pseudo-daemon)
; (osx-pseudo-daemon-mode 1)

;; Hooks
(add-hook 'js-mode-hook (lambda () (flycheck-mode t)))
(add-hook 'python-mode-hook (lambda () (flycheck-select-checker "javascript-eslint")))

;; Evil
;; Use c-u as in vim. (This has to come before we require the evil mode)
(setq evil-want-C-u-scroll t)
;; Evil search
(setq evil-search-module 'evil-search
      evil-want-C-u-scroll t
      evil-want-C-w-in-emacs-state t)

; (require 'evil)
(evil-mode t)

;; Load up the actual configuration files
(load "~/.emacs.d/user.el")
(load "~/.emacs.d/evil.el")
(load "~/.emacs.d/style.el")
(load "~/.emacs.d/eshell-conf.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("cd540cb356cb169fa1493791bd4cbb183c5ad1c672b8d1be7b23e5e8c8178991" "9e54a6ac0051987b4296e9276eecc5dfb67fdcd620191ee553f40a9b6d943e78" "cf08ae4c26cacce2eebff39d129ea0a21c9d7bf70ea9b945588c1c66392578d1" "5ee12d8250b0952deefc88814cf0672327d7ee70b16344372db9460e9a0e3ffc" "52588047a0fe3727e3cd8a90e76d7f078c9bd62c0b246324e557dfa5112e0d0c" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(evil-emacs-state-modes
   (quote
    (magit-commit-mode magit-log-mode magit-stash-mode magit-status-mode)))
 '(js2-basic-offset 2)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

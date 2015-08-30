;; First require Marmalade repo
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;;(when (not package-archive-contents)
;;    (package-refresh-contents))

;; Install starter packages if not there already.
;; Starter package link: https://github.com/technomancy/emacs-starter-kit
(setq package-enable-at-startup nil)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/packages/")

;; load file helper functions
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

;; OS X: Pseudo Daemon mode: Create a new frame every time
;; we exit from an old one, for emacsclient, so OS X doc icon doesn't become
;; useless.
                                        ; (require 'osx-pseudo-daemon)
                                        ; (osx-pseudo-daemon-mode 1)

;; Hooks

;; Flycheck
(add-hook 'js2-mode-hook (lambda () (flycheck-mode t)))
(add-hook 'go-mode (lambda () (flycheck-mode t)))
(add-hook 'flycheck-mode (lambda () (flycheck-disable-checker "javascript-eslint")))
(add-hook 'flycheck-mode (lambda () (setq flycheck-jshintrc "~/.jshintrc")))

;; Evil
;; Use c-u as in vim. (This has to come before we require the evil mode)
(setq evil-want-C-u-scroll t)
;; Evil search
(setq evil-search-module 'evil-search
      evil-want-C-u-scroll t
      evil-want-C-w-in-emacs-state t)

;; Enable evil-leader, before evil itself.
(autoload 'evil-leader "Evil leader mode" "Minor mode for evil" t)
(global-evil-leader-mode)
(evil-leader/set-leader ";")
;; (require 'evil)
(load-user-file "internal-config.el")
(load-user-file "evil.el")
(evil-mode t)

;; Wakatime
;; (global-wakatime-mode)

;; Path
(add-to-list 'load-path "~/.emacs.d/packages/")
(add-to-list 'load-path "~/.emacs.d/themes")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Load up the actual configuration files
(load-user-file "eshell-conf.el")
(load-user-file "user.el")
(load-user-file "ido.el")
(load-user-file "utilities.el")
(load-user-file "style.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "79a3f477ac0cb4a106f78b6109614e991564a5c2467c36e6e854d4bc1102e178" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "cab317d0125d7aab145bc7ee03a1e16804d5abdfa2aa8738198ac30dc5f7b569" "3e6c74baadecb6343d472ef8f8101b99b18f9caf5faf288f78a7659e88508fc7" "bc7e7879f682c176bd06134fa5bded66a5381a69b8f11fce3d0713fa9107d93e" "e20210182a77631882d6b0e6f6cb9c273e00623200acfd436361cdc8430a7e22" "d74183b099f4e91052941ef3131c76697caae3fcf581f4c140216a7c6e6d71e2" "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "46fd293ff6e2f6b74a5edf1063c32f2a758ec24a5f63d13b07a20255c074d399" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "7bde52fdac7ac54d00f3d4c559f2f7aa899311655e7eb20ec5491f3b5c533fe8" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "8b30636c9a903a9fa38c7dcf779da0724a37959967b6e4c714fdc3b3fe0b8653" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "5ee12d8250b0952deefc88814cf0672327d7ee70b16344372db9460e9a0e3ffc" "cf08ae4c26cacce2eebff39d129ea0a21c9d7bf70ea9b945588c1c66392578d1" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "9e54a6ac0051987b4296e9276eecc5dfb67fdcd620191ee553f40a9b6d943e78" default)))
 '(evil-emacs-state-modes
   (quote
    (magit-commit-mode magit-log-mode magit-stash-mode magit-status-mode org-agenda-mode)))
 '(magit-commit-arguments nil)
 '(wakatime-api-key "8ef07422-e38c-46d5-be7d-9c466f2a49d5")
 '(wakatime-cli-path "/usr/local/bin/wakatime"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; First require Marmalade repo
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)

(load "~/.emacs.d/user.el")

;; Install starter packages if not there already.
;; Starter package link: https://github.com/technomancy/emacs-starter-kit
(package-initialize)

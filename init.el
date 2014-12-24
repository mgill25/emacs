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
(package-initialize)

;; Path
(add-to-list 'load-path "~/.emacs.d/packages/")

;; Then installed packages: better-defaults; magit; paredit
;; Install helm. It is amazing. Holy shit helm is AMAZING.

(global-set-key (kbd "M-x") 'helm-M-x) ; Primary helm

;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Find in project keybinding.
; (global-set-key (kbd "C-x f") 'find-file-in-project)
(global-set-key (kbd "C-x f") 'helm-find-files)

;; helm: http://tuhdo.github.io/helm-intro.html
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

;; Currently open buffer list
(global-set-key (kbd "C-x b") 'helm-buffers-list)

;; Using The Silver Searcher with Helm
(when (executable-find "ag")
  (setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
  (setq helm-ag-command-option "--all-text")
  (setq helm-ag-insert-at-point 'symbol)
  (global-set-key (kbd "M-s") 'helm-do-ag))

(helm-mode 1)

;; Helm Git
(require 'helm-ls-git)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)

;; Linum
(require 'linum)
(global-linum-mode t)
(setq linum-format "%3d ")

;; Paredit for Lispy files.
;; TODO: enable-paredit-mode not working right now.

;; (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;; (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;; (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;; (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;; (add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; ----------------------------------------------------------------------------------------------

;; Other non-mode settings for the editor.

;; Use spaces instead of tabs for indentation
(setq indent-tabs-mode nil)

;; Set backup directory location
(setq backup-directory-alist '(("." . "~/.emacs.d/saves/")))

;; Make backups by copying
(setq backup-by-copying-when-linked t)

(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; No easy way to jump to matching parenthesis, so we make a mapping!
;; Just like in vim, we use the % sign to jump to matching paren.
(global-set-key "%" 'match-paren)

(defun match-paren (arg)
 "Go to the matching paren if on a paren; otherwise, insert %."
(interactive "p")
(cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
      ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
      (t (self-insert-command (or arg 1)))))


;; Path
(add-to-list 'load-path "~/.emacs.d/packages/")

;; Emacs System stuff
(defalias 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "C-+") 'text-scale-increase) ; Increase font size
(global-set-key (kbd "C--") 'text-scale-decrease) ; Decease font size

(setq mac-option-modifier 'super) ;; OS X option key is now super
(setq mac-command-modifier 'meta) ;; OS X cmd key is now Meta

;; Editor stuff

;; Linum
(require 'linum)
(global-linum-mode t)
(setq linum-format "%3d ")

;; Indentation
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Use OS clipboard
(require 'pbcopy)
(turn-on-pbcopy)

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

;; Ignore bell
(setq ring-bell-function 'ignore)

;; No easy way to jump to matching parenthesis, so we make a mapping!
;; Just like in vim, we use the % sign to jump to matching paren.
(global-set-key "%" 'match-paren)
(defun match-paren (arg)
 "Go to the matching paren if on a paren; otherwise, insert %."
(interactive "p")
(cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
      ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
      (t (self-insert-command (or arg 1)))))

;; Then installed packages: better-defaults; magit; paredit
;; Install helm. It is amazing. Holy shit helm is AMAZING.

(global-set-key (kbd "M-x") 'helm-M-x) ; Primary helm

;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Find in project keybinding.
; (global-set-key (kbd "C-x f") 'find-file-in-project)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; Currently open buffer list
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
;; Kill Ring
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; Helm-mini
(global-set-key (kbd "M-p") 'helm-mini)

;; Using The Silver Searcher with Helm
(when (executable-find "ag")
  (setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
  (setq helm-ag-command-option "--all-text")
  (setq helm-ag-insert-at-point 'symbol)
  (global-set-key (kbd "M-s") 'helm-do-ag))

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

;; helm: http://tuhdo.github.io/helm-intro.html
(require 'helm)
(require 'helm-config)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

;; Helm Git keybinding
(global-set-key (kbd "M-m") 'helm-browse-project)
(global-set-key (kbd "C-x M-s") 'helm-ls-git-ls)

;(when (executable-find "curl")
;  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

(require 'helm-ls-git)
(helm-mode 1)

;; Paredit for Lispy files.
;; TODO: enable-paredit-mode not working right now.
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; ----------------------------------------------------------------------------------------------

;; SML-Mode
(autoload 'sml-mode "~/.emacs.d/packages/sml-mode-6.5.el"
  "Major mode for editing SML." t)
(setq auto-mode-alist
      (cons '("\.sml$" . sml-mode)
	    auto-mode-alist))
(setq sml-program-name "/usr/local/bin/sml")
(add-hook 'sml-mode-hook 'electric-indent-mode)
;; Hook for quickly restarting REPL and loading current file
(require 'cl)
(add-hook 'sml-mode-hook
          (lambda ()
            (define-key sml-mode-map (kbd "C-c C-v")
              'my-sml-restart-repl-and-load-current-file)
            (defun my-sml-restart-repl-and-load-current-file ()
              (interactive)
              (ignore-errors
                (with-current-buffer "*sml*"
                  (comint-interrupt-subjob)
                  (comint-send-eof)
                  (let ((some-time 0.1))
                    (while (process-status (get-process "sml"))
                      (sleep-for some-time)))))
              (cl-flet ((sml--read-run-cmd ()
                                           '("/usr/local/bin/sml" "" nil)))   ; (command args host)  
                (sml-prog-proc-send-buffer t)))))



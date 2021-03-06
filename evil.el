;;;;;;;;;; Evil Mode Config ;;;;;;;;;;;;

;; Esc quits
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; From: http://blog.jakubarnold.cz/2014/06/23/evil-mode-how-to-switch-from-vim-to-emacs.html
(define-key evil-normal-state-map (kbd ",,") 'evil-buffer)
(define-key evil-normal-state-map (kbd "q") nil) ;; kbd macro

(define-key evil-normal-state-map (kbd "C-p") 'helm-projectile) ;; use helm on Ctrl

(eval-after-load 'evil-mode '(define-key evil-insert-state-map (kbd "C-e") nil))
(define-key evil-insert-state-map (kbd "C-d") nil)
(define-key evil-insert-state-map (kbd "C-k") nil)
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-visual-state-map (kbd "C-c") 'evil-normal-state)

(define-key evil-motion-state-map (kbd "C-e") nil)
(define-key evil-visual-state-map (kbd "C-c") 'evil-exit-visual-state)

;; navigation for splits
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

;; Define Ex Mode Commands for Evil
(evil-ex-define-cmd "h[ide]" 'bury-buffer)
(evil-ex-define-cmd "colo[rscheme]" 'load-theme)
(evil-ex-define-cmd "bd" 'evil-delete-buffer)

;; Org mode
;; (evil-define-key 'normal org-mode-map (kbd "]n") 'org-forward-heading-same-level)
;; (evil-define-key 'normal org-mode-map (kbd "[n") 'org-backward-heading-same-level)

;; (evil-define-key 'normal org-mode-map (kbd "C-M-l") 'org-shiftright)
;; (evil-define-key 'normal org-mode-map (kbd "C-M-h") 'org-shiftleft)
;; (evil-define-key 'insert org-mode-map (kbd "C-M-l") 'org-shiftright)
;; (evil-define-key 'insert org-mode-map (kbd "C-M-h") 'org-shiftleft)

;; Leader!
;; keyboard shortcuts
(evil-leader/set-key
  "a" 'ag-project
  "A" 'ag
  "b" 'ido-switch-buffer
  "B" 'ido-switch-buffer-other-window
  "c" 'mc/mark-next-like-this
  "C" 'mc/mark-all-like-this
  "d" 'dired-jump
  "D" 'ido-dired
  "e" 'er/expand-region
  "E" 'mc/edit-lines
  "f" 'ido-find-file
  "g" 'magit-status
  "G" 'magit-blame
  "i" 'idomenu
  "j" 'ace-jump-mode
  "J" 'ace-jump-word-mode
  "k" 'kill-this-buffer
  "K" 'kill-buffer
  "l" 'linum-mode
  "L" 'linum-relative-toggle
  "m" 'helm-mini
  "o" 'occur
  "O" 'browse-url
  "p" 'magit-find-file-completing-read
  "P" 'popwin:popup-last-buffer
  "r" 'bw/recentf-ido-find-file
  "R" 'bookmark-jump
  "s" 'ag-project
  "t" 'bw/open-term
  "T" 'eshell
  "w" 'save-buffer
  "x" 'smex
  "y" 'bury-buffer)

;; Evil settings for specific modes.
(evil-set-initial-state 'magit-log-edit-mode 'insert)
(add-to-list 'evil-buffer-regexps '("\\*magit:"))

(evil-add-hjkl-bindings ag-mode-map 'normal
    "n"   'evil-search-next
    "N"   'evil-search-previous
    "RET" 'compile-goto-error)

(evil-add-hjkl-bindings org-agenda-mode-map 'emacs
    "RET" 'org-agenda-switch-to)

;; Custom variable
;; Add any mode that should start in emacs mode instead of normal/insert mode here!
(custom-set-variables
 '(evil-emacs-state-modes
   (quote (magit-commit-mode
           magit-log-mode
           magit-stash-mode
           magit-status-mode))))

;; on OSX, stop copying each visual state move to the clipboard:
;; https://bitbucket.org/lyro/evil/issue/336/osx-visual-state-copies-the-region-on
;; Most of this code grokked from:
;; http://stackoverflow.com/questions/15873346/elisp-rename-macro
(defadvice evil-visual-update-x-selection (around clobber-x-select-text activate)
  (fset 'old-x-select-text (symbol-function 'x-select-text))
  (fmakunbound 'x-select-text)
  ad-do-it
  (fset 'x-select-text (symbol-function 'old-x-select-text)))

;; Relative linum
(after-load 'linum-relative
  (setq linum-format "%3d ")
  (defun bw/linum-non-relative (line-number)
    "Linum formatter that copies the format"
    (propertize (format linum-relative-format line-number) 'face 'linum))

  (defun bw/linum-relative-formatting ()
    "Turn on relative formatting"
    (setq-local linum-format 'linum-relative))

  (defun bw/linum-normal-formatting ()
    "Turn on non-relative formatting"
    (setq-local linum-format 'bw/linum-non-relative))

  ;; I never use linum-mode except for this, so it's okay to
  ;; clobber it
  (setq linum-format 'bw/linum-non-relative
        ;; show >> on line where cursor is
        linum-relative-current-symbol ">>")

  ; in Normal mode, use relative numbering
  ;;(add-hook 'evil-normal-state-entry-hook 'bw/linum-relative-formatting)
  ; in Insert mode, use normal line numbering
  ;;(add-hook 'evil-insert-state-entry-hook 'bw/linum-normal-formatting)
  ; turn off linum mode automatically when entering Emacs mode
  ;;(add-hook 'evil-emacs-state-entry-hook 'bw/disable-linum-mode)
  ; turn off linum mode when entering Emacs
  ;;(add-hook 'evil-emacs-state-entry-hook 'bw/linum-normal-formatting)

  ;; copy linum face so it doesn't look weird
  (set-face-attribute 'linum-relative-current-face nil :foreground (face-attribute 'font-lock-keyword-face :foreground) :background nil :inherit 'linum :bold t))

(autoload 'linum-relative-toggle "linum-relative" nil t)

;; Linum
;;(autoload 'linum "linum-mode" "display line number on each buffer" t)
(global-linum-mode t)
(eval-after-load 'linum-mode '(set-face-attribute 'linum nil :height 100))
(eval-after-load 'linum-mode '(setq linum-format "%3d "))
;;(setq linum-format "%3d \u2502 ") ;; With dashes as separator

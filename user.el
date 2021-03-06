;; Emacs System stuff
(defalias 'yes-or-no-p 'y-or-n-p)
(setq visible-bell nil)
(setq ring-bell-function #'ignore)

;; Disable some modes if files are heavy
(defun conditional-disable-modes ()
  (when (> (buffer-size) 2000000)
    (flycheck-mode -1)
    (linum-mode -1)))

(add-hook 'c-mode-hook 'conditional-disable-modes)
(add-hook 'c++-mode-hook 'conditional-disable-modes)

(setq mac-option-modifier 'super) ;; OS X option key is now super
(setq mac-command-modifier 'meta) ;; OS X cmd key is now Meta

;; Better modeline
(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))


;; Editor stuff

;; Indentation
(define-key global-map (kbd "RET") 'newline-and-indent)
(setq python-indent-level 4)
(setq js-indent-level 2)
(setq ruby-indent-level 2)

;; Aggressive auto-indentation
;; taken from https://github.com/howardabrams/dot-files/blob/master/emacs.org

(defun indent-defun ()
  "Indent current defun.
Do nothing if mark is active (to avoid deactivaing it), or if
buffer is not modified (to avoid creating accidental
modifications)."
  (interactive)
  (unless (or (region-active-p)
              buffer-read-only
              (null (buffer-modified-p)))
    (let ((l (save-excursion (beginning-of-defun 1) (point)))
          (r (save-excursion (end-of-defun 1) (point))))
      (cl-letf (((symbol-function 'message) #'ignore))
        (indent-region l r)))))

(defun activate-aggressive-indent ()
  "Locally add `ha/indent-defun' to `post-command-hook'."
  (add-hook 'post-command-hook
            'indent-defun nil 'local))

;;(add-hook 'emacs-lisp-mode-hook 'activate-aggressive-indent)
;;(add-hook 'python-mode-hook 'activate-aggressive-indent)

;; M-( can insert () pair. Do the same for others.
;; (global-set-key (kbd "M-[") 'insert-pair)

(global-set-key (kbd "M-{") 'insert-pair)
(global-set-key (kbd "M-<") 'insert-pair)
(global-set-key (kbd "M-'") 'insert-pair)
(global-set-key (kbd "M-`") 'insert-pair)
(global-set-key (kbd "M-\"") 'insert-pair)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(global-set-key (kbd "C-M-;") 'avy-goto-char)
(global-set-key (kbd "C-M-'") 'avy-goto-char-2)

;; use os clipboard
(autoload 'turn-on-pbcopy "pbcopy" "Minor mode" t)
(turn-on-pbcopy)

;; use spaces instead of tabs for indentation
(setq indent-tabs-mode nil)

;; set backup directory location
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


;; Turn on regular expressions for ISearch
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Then installed packages: better-defaults; magit; paredit
;; Install helm. It is amazing. Holy shit helm is AMAZING.

(global-set-key (kbd "M-x") 'helm-M-x) ; Primary helm

;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Find in project keybinding.
;; (global-set-key (kbd "C-x f") 'find-file-in-project)
(global-set-key (kbd "M-o") 'helm-find-files)

;; Currently open buffer list
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
;; Kill Ring
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; Helm-mini
(global-set-key (kbd "M-b") 'helm-mini)

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
(require 'helm-buffers)

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(global-set-key (kbd "M-p") 'helm-projectile)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

;; Helm Git keybinding
(global-set-key (kbd "M-m") 'helm-browse-project)
(global-set-key (kbd "C-x M-s") 'helm-ls-git-ls)

;;(when (executable-find "curl")
;;  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)

(global-set-key (kbd "M-t") 'helm-semantic-or-imenu)

;;(require 'helm-ls-git)

(autoload 'helm-ls-git-ls "helm-ls-git" "\
\(fn &optional ARG)" t nil)

(helm-mode 1)

;; Recent file list. Emacs already has this feature builtin
(autoload 'recentf-mode "recentf" "minor" t)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-c f f") 'recentf-open-files)

;; Org-mode settings
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done 'time) ;; Insert timestamp right after a TODO field is finished.
(setq org-agenda-files (list "~/org/work.org"
                             "~/org/home.org"))
(setq org-src-fontify-natively t)

(defun linevich-linum-mode()  
  "Custom view for linum mode in text editing"  
  (interactive)  
  (make-local-variable 'linum-format)
  (setq linum-format " "))
(add-hook 'org-mode-hook 'linevich-linum-mode)  

;; automatically open the work.org file when emacs starts
;; (find-file "~/org/work.org")

;; Paredit for Lispy files.
;; TODO: enable-paredit-mode not working right now.
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; auto-complete mode
;;(require 'auto-complete-config)
;;(ac-config-default)

;; ~~Javascript settings.~~
(setq js-basic-indent 2)
(setq-default js2-basic-indent 2)

(setq-default js2-basic-offset 2)
(setq-default js2-auto-indent-p t)
(setq-default js2-cleanup-whitespace t)
(setq-default js2-enter-indents-newline t)
(setq-default js2-global-externs "jQuery $")
(setq-default js2-indent-on-enter-key t)
(setq-default js2-mode-indent-ignore-first-tab t)
(setq-default js2-global-externs '("module"
                                   "require"
                                   "buster"
                                   "sinon"
                                   "assert"
                                   "refute"
                                   "setTimeout"
                                   "clearTimeout"
                                   "setInterval"
                                   "clearInterval"
                                   "location"
                                   "__dirname"
                                   "console"
                                   "JSON"))
;; We'll let fly do the error parsing...
;;(setq-default js2-show-parse-errors nil)

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; (add-to-list 'auto-mode-alist '("\\.es$" . js2-mode))
(add-hook 'js2-mode-hook 'conditional-disable-modes)
(rename-modeline "js2-mode" js2-mode "JS2")

;; restclient mode mapping
(add-to-list 'auto-mode-alist '("\\.restclient$" . restclient-mode))

(add-hook 'js-mode-hook 'js2-minor-mode)
;;(add-hook 'js2-mode-hook ac-js2-mode)
(setq js2-highlight-level 3)

;; Place warning font arount this stuff
(font-lock-add-keywords 'js2-mode
                        '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
                           1 font-lock-warning-face t)))

;; JS2-Refactor prefix keybinding
;; https://github.com/magnars/js2-refactor.el
;;(js2r-add-keybindings-with-prefix "C-c C-m")
;; eg. extract function with `C-c C-m ef`.
;; js-comint mode, combined with js-2 makes great interactive node experience, apparently!
(autoload 'js-comint "js-comint"
  "Hooking JavaScript interpreter up to the JS Files." t nil)
(setenv "NODE_NO_READLINE" "1")   ;; Turn off fancy node prompt
;; Use node as our repl
(setq inferior-js-program-command "node")

(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        ;; Deal with some prompt nonsense
        (add-to-list
         'comint-preoutput-filter-functions
         (lambda (output)
           (replace-regexp-in-string "\033\\[[0-9]+[GK]" "" output)
           (replace-regexp-in-string ".*1G.*3G" "&GT;" output)
           (replace-regexp-in-string "&GT;" "> " output)))))

(autoload 'nodejs-repl "nodejs-repl" "Major mode" t)
(defun my/js-keybindings ()
  (interactive)
  (local-set-key (kbd "C-x C-r") 'nodejs-repl-start-and-eval-region)
  (local-set-key (kbd "C-x C-e") 'nodejs-repl-eval-buffer)
  (local-set-key (kbd "C-x C-j") 'nodejs-repl-eval-dwim)
  (local-set-key (kbd "C-x C-l") 'nodejs-repl))

(add-hook 'js-mode-hook 'my/js-keybindings)
(add-hook 'js2-mode-hook 'my/js-keybindings)
(eval-after-load 'js2-mode '(define-key js2-mode-map (kbd "C-x C-e") 'send-region-to-nodejs-repl-process))
(eval-after-load 'nodejs-repl '(define-key nodejs-repl-mode-map (kbd "<up>") 'comint-previous-input))
(eval-after-load 'nodejs-repl '(define-key nodejs-repl-mode-map (kbd "<down>") 'comint-next-input))
;; ------------ ~Fin Javascript ---------------

;; Turn on snippets
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets/"))
;;(yas-global-mode 1)
(eval-after-load 'yasnippet (yas-reload-all))
(add-hook 'js2-mode-hook #'yas-minor-mode)
(add-hook 'python-mode-hook #'yas-minor-mode)
(add-to-list 'yas/root-directory "~/.emacs.d/snippets/yasnippet-snippets/")
;;(yas/initialize)

;; auto complete mode
;; should be loaded after yasnippet so that they can work together
(autoload 'auto-complete "auto-complete-mode" nil t)
(eval-after-load 'auto-complete-mode '(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict"))
(eval-after-load 'auto-complete-mode '(ac-config-default))

;; set the trigger key so that it can work together with yasnippet on tab key,
;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;; activate, otherwise, auto-complete will
(eval-after-load 'auto-complete-mode '(ac-set-trigger-key "TAB"))
(eval-after-load 'auto-complete-mode '(ac-set-trigger-key "<tab>"))

;; Expand region: Text selection by semantic units.
(autoload 'expand-region "expand-region" "minor" t)
(eval-after-load 'expand-region '(global-set-key (kbd "C-@") 'er/expand-region))
;; ----------------------------------------------------------------------------------------------

;; Web beautify settings.
;; Set up paredit with js2-mode
(eval-after-load 'js2-mode '(define-key js2-mode-map "{" 'paredit-open-curly))
(eval-after-load 'js2-mode '(define-key js2-mode-map "}" 'paredit-close-curly-and-newline))
(eval-after-load 'js2-mode '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
;;(eval-after-load 'js '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
(eval-after-load 'json-mode '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
(eval-after-load 'sgml-mode '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))
(eval-after-load 'css-mode '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))


;; Abbreviation mode (builtin)
(setq-default abbrev-mode t) 
(setq save-abbrevs nil) ;; stop asking to save newly added abbrevs on quitting.

(define-abbrev-table 'global-abbrev-table
  '(("8mg" "@mgill25")
    ("8name" "Manish Gill")
    ("8btw" "by the way")
    ("8note" "NOTE: ")
    ("8todo" "TODO: ")
    ("8js" "JavaScript")
    ("8py" "Python")
    ("8py3" "Python3")
    ("8wd" "Workday")
    ("8we" "Weekend")))

;; Note: Capitalizing the first letter, i.e. Btw, expands the abbreviation with an initial capital, i.e. By the way … Sweet.

;; Emmet-Mode
(autoload 'emmet-mode "emmet-mode" "minor" t)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
;; Prettify symbols globally
(when (fboundp 'global-prettify-symbols-mode)
  (defconst lisp--prettify-symbols-alist
    '(("lambda"       . ?λ)
      ("curry"        . ?»)
      ("rcurry"       . ?«)
      ("comp"         . ?∘)
      ("compose"      . ?∘)
      ("."            . ?•)))
  (global-prettify-symbols-mode 1))

;; Words with dashes don't separate words in lisp
(dolist (c (string-to-list ":_-?!#*"))
  (modify-syntax-entry c "w" emacs-lisp-mode-syntax-table))

;; JavaScript 
(when (fboundp 'global-prettify-symbols-mode)
  (add-hook 'js2-mode-hook
            (lambda ()
              (push '("function" . ?ƒ) prettify-symbols-alist)
              (modify-syntax-entry ?. "."))))
;; Python
(when (fboundp 'global-prettify-symbols-mode)
  (add-hook 'python-mode-hook
            (lambda ()
              (push '("self" . ?◎) prettify-symbols-alist)
              (modify-syntax-entry ?. "."))))
;; Python-Jedi
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional


;; Whitespace http://www.emacswiki.org/emacs/WhiteSpace
;; Don't use tabs. That way lies madness!
(setq-default indent-tabs-mode nil)
(autoload 'whitespace-mode "whitespace-mode" "minor" t)
;;(setq whitespace-style '(tabs tab-mark)) ;turns on white space mode only for tabs
(setq whitespace-style '()) ;turns on white space mode only for tabs
(global-whitespace-mode 1)

;; Golang: go-mode
(require 'go-mode-autoloads)
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c i") 'go-goto-imports)))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")

;; Frame

(setq frame-resize-pixelwise t)
(toggle-frame-maximized)

;; Themes

;; (load-theme 'adwaita t)
;; (load-theme 'tomorrow-night t)
;; (load-theme 'tomorrow-night-bright t)
;; (load-theme 'solarized-dark t)
;; (load-theme 'solarized-dark t)
;; (load-theme 'cyberpunk t)

(load-theme 'tomorrow-night-bright t t) ; last t is for NO ENABLE

;; Function to only load the theme in GUI for emacsclient
;; don't load any theme in console.
;; Ref: http://superuser.com/a/600357
(defun mb/pick-color-theme (frame)
  (select-frame frame)
  (if (window-system frame)
      (progn (enable-theme 'tomorrow-night-bright))
    (progn
      (menu-bar-mode 0)
      (disable-theme 'tomorrow-night-bright))))
(add-hook 'after-make-frame-functions 'mb/pick-color-theme)

;; For when started with emacs or emacs -nw rather than emacs --daemon
(if window-system (enable-theme 'tomorrow-night-bright))

;; Font
(set-frame-font "Monaco-15:width=condensed")      ; Set font for current frame
;; Sets default font for all the frames
(add-to-list 'default-frame-alist
             '(font . "Monaco-15:width=condensed"))
(set-face-attribute 'default nil :height 150)
(setq font-lock-maximum-decoration t)

;; Disable menu bar
(menu-bar-mode 0)

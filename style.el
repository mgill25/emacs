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

(load-theme 'junio t t) ; last t is for NO ENABLE

;; Function to only load the theme in GUI for emacsclient
;; don't load any theme in console.
;; Ref: http://superuser.com/a/600357
(defun mb/pick-color-theme (frame)
  (select-frame frame)
  (if (window-system frame)
      (progn (enable-theme 'junio))
    (progn
      (menu-bar-mode 0)
      (disable-theme 'junio))))
(add-hook 'after-make-frame-functions 'mb/pick-color-theme)

;; For when started with emacs or emacs -nw rather than emacs --daemon
(if (window-system) (enable-theme 'junio))

;; Font
(set-frame-font "Monaco-13:width=condensed")      ; Set font for current frame
;; Sets default font for all the frames
(add-to-list 'default-frame-alist
             '(font . "Monaco-13:width=condensed"))
(set-face-attribute 'default nil :height 150)
(setq font-lock-maximum-decoration t)

;; Disable menu bar, tool bar, scroll bar.
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode -1)

;; Start the frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Modeline
(setq sml/no-confirm-load-theme t)
(sml/setup)

;; -------------- Better-Zoom ------------------

;; This script is set for a `text-scale-mode-step` of `1.04`
(setq text-scale-mode-step 1.04)
;;
;; List: `Sub-Zoom Font Heights per text-scale-mode-step`  
;;   eg.  For a default font-height of 120 just remove the leading `160 150 140 130` 
(defvar sub-zoom-ht (list 160 150 140 130 120 120 110 100 100  90  80  80  80  80  70  70  60  60  50  50  50  40  40  40  30  20  20  20  20  20  20  10  10  10  10  10  10  10  10  10  10   5   5   5   5   5   2   2   2   2   2   2   2   2   1   1   1   1   1   1   1   1   1   1   1   1))
(defvar sub-zoom-len (safe-length sub-zoom-ht))
(defvar def-zoom-ht (car sub-zoom-ht))
(set-face-attribute 'default nil :height def-zoom-ht)

(defun text-scale-adjust-zAp ()
   (interactive)
   (text-scale-adjust 0)
 )
(global-set-key [C-kp-multiply] 'text-scale-adjust-zAp)

(defun text-scale-decrease-zAp ()
   (interactive)
   (if (not (boundp 'text-scale-mode-amount)) ;; first-time init  
              (setq  text-scale-mode-amount 0))
   (setq text-scale (round (/ (* 1 text-scale-mode-amount) 
                                   text-scale-mode-step)))
   (if (> text-scale (- 1 sub-zoom-len))
       (progn
         (text-scale-decrease text-scale-mode-step))))

;; Linum mode constant height
(eval-after-load "linum"
  '(set-face-attribute 'linum nil :height 100))

(global-set-key [C-kp-subtract] 'text-scale-decrease-zAp)

(defun text-scale-increase-zAp ()
   (interactive)
   (if (not (boundp 'text-scale-mode-amount)) ;; first-time init  
              (setq  text-scale-mode-amount 0))
   (setq text-scale (round (/ (* 1 text-scale-mode-amount) 
                                   text-scale-mode-step)))
   (if (< text-scale 85)
       (progn
         (text-scale-increase text-scale-mode-step))))
         
(global-set-key [C-kp-add] 'text-scale-increase-zAp)
;; Zoom font via Numeric Keypad
;(global-set-key [C-kp-multiply] 'text-scale-adjust-zAp)
;(global-set-key [C-kp-subtract] 'text-scale-decrease-zAp)
;(global-set-key [C-kp-add]      'text-scale-increase-zAp)

(global-set-key (kbd "M-0") 'text-scale-adjust-zAp)
(global-set-key (kbd "M--") 'text-scale-decrease-zAp)
(global-set-key (kbd "M-=") 'text-scale-increase-zAp)

;; Zoom font via Control Mouse Wheel
(global-set-key (kbd "<C-mouse-4>") 'text-scale-increase-zAp)
(global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease-zAp)

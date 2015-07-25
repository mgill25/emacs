;; Graveyard. This is the place where I dump things that I don't
;; need right now, but might come in handy in future.

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

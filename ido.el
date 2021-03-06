;; Ido-mode configuration
(after-load 'ido
  (setq
   ;; Speed up ido by using less candidates
   ido-max-prospects 10
   ;; Match arbitrary points in strings
   ido-enable-prefix nil
   ;; Match across entire string
   ido-enable-flex-matching t
   ;; Create a new buffer if there's no match candidate
   ido-create-new-buffer 'always
   ;; Don't try and guess if the string under point is a file
   ido-use-filename-at-point nil
   ;; case-insensitive matching
   ido-case-fold t
   ;; don't store old files as virtual buffers
   ido-use-virtual-buffers nil
   ;; enable ido everywhere
   ido-everywhere 1))

(setq confirm-nonexistent-file-or-buffer nil)

(autoload 'ido-mode "ido" "minor" t)
(ido-mode 1)


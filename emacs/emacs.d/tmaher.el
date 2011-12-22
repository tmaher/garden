(setq
 make-backup-files nil
 next-line-add-newlines nil  ;;; rms is a twit
 delete-key-deletes-forward nil
 minibuffer-max-depth nil
 default-fill-column 72
 default-tab-width 8
 require-final-newline t
 mm-inline-media-tests 
 )

(setq-default
 indent-tabs-mode nil
 sh-indentation 2
 )

(defun tmaher-init-keybind ()
  "Rebind my keys correctly, damn it!"
  (interactive)
  (global-set-key "\C-x\-" 'shrink-window)
  (global-set-key "\M-g" 'goto-line)
  (global-set-key "\C-g" 'jolly-candy-like-keyboard-quit)
  (global-set-key "\C-s" 'isearch-forward-regexp)
  (global-set-key "\C-r" 'isearch-backward-regexp)
  (global-set-key "\M-%" 'query-replace-regexp)
  (global-set-key [(meta tab)] 'dabbrev-completion)
  (global-set-key "\M-s" 'ispell-buffer)
  (define-key global-map [(control home)] 'beginning-of-buffer)
  (define-key global-map [(control end)] 'end-of-buffer)
  (define-key global-map [home] 'beginning-of-line)
  (define-key global-map [end] 'end-of-line)
  )

(defun crack-whore () 
  "It's easy, m'kay?"
  (interactive)
  (message "Living on the street, giving handjobs for crack."))

(defun jolly-candy-like-keyboard-quit ()
  (interactive)
  (signal 'quit
          '("Don't touch it! It's the History Eraser Button, you fool!")))

(defun indent-buffer () (interactive)
  "Smart-indent buffer"
  (save-excursion (indent-region (point-min) (point-max) nil))
  (save-excursion (untabify (point-min) (point-max)))
  (if font-lock-mode (font-lock-fontify-buffer)))

(defalias 'perl-mode 'cperl-mode)

(setq auto-mode-alist
      (append (list '("\\.pl$" . cperl-mode)
                    '("\\.txt$" . text-mode)
                    '("\\.+rc$" . shell-script-mode)
                    '("\\.as$" . actionscript-mode)
                    ) auto-mode-alist)
      )

(column-number-mode t)
(line-number-mode t)

(add-hook 'java-mode-hook (lambda () (setq c-basic-offset 2
                                      indent-tabs-mode nil)))

(server-start)
(add-hook' term-setup-hook (lambda () (tmaher-init-keybind)))
(add-hook' term-setup-hook (lambda ()  (message "What is thy bidding, my master?")))

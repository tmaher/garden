(if (boundp 'tmaher-system-load-path) () 
  (defvar tmaher-system-load-path load-path))
(defvar tmaher-load-path 
  '("~/.emacs.d/misc"
    "~/.emacs.d/zephyr"
    ;;; "~/.emacs.d/nxhtml"
    ))
(setq load-path (append tmaher-load-path tmaher-system-load-path))

;; emacsclient flails if this isn't defined
(if (functionp 'tool-bar-mode) (tool-bar-mode 0)
  (defun tool-bar-mode (x) "tmaher unimplemented stub" nil)
  )

(require 'cl)
(require 'timer)
(require 'vc)
(require 'actionscript-mode)
(require 'zephyr-setup)
(load "~/.emacs.d/nxhtml/autostart")

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(load-home-init-file t t)
 '(nxhtml-skip-welcome t)
 '(query-user-mail-address nil)
 '(toolbar-visible-p nil)
 '(user-mail-address "Tom Maher <tmaher@tursom.org>"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(font-lock-comment-delimiter-face ((default (:inherit font-lock-comment-face)) (((class color) (min-colors 8) (background dark)) nil)))
 '(font-lock-comment-face ((nil (:foreground "red"))))
 '(link ((t (:foreground "cyan" :underline t))))
 '(minibuffer-prompt ((t (:foreground "cyan"))))
 '(mode-line ((t (:background "blue" :foreground "yellow"))))
 '(mumamo-background-chunk-major ((((class color) (min-colors 8)) nil)))
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 8)) nil))))
(put 'ido-exit-minibuffer 'disabled nil)


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

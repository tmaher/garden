(if (boundp 'tmaher-system-load-path) () 
  (defvar tmaher-system-load-path load-path))
(defvar tmaher-load-path 
  '("~/.emacs.d/misc"
    ;;; "~/.emacs.d/nxhtml"
    ))
(setq load-path (append tmaher-load-path tmaher-system-load-path))

(require 'cl)
(require 'timer)
(require 'vc)
(require 'actionscript-mode)
(load "~/.emacs.d/nxhtml/autostart")

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

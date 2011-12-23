;;; magus-macs:  A library of useful functions.
;;; Based on the .emacs file of Nat Lanza <magus@cs.cmu.edu>

;;; Last modified:  25 June 1999

(require 'cl)

(load "cl-macs")

;;; (setq frame-title-format (format "%%S@%s: %%b" (system-name)))

(defvar magus-in-xemacs (string-match "XEmacs" emacs-version)
  "`t' if we are in XEmacs.  Default is `nil', obviously.")
(defmacro fsf/x (fsf x) `(if magus-in-xemacs ,x ,fsf))

(if magus-in-xemacs (require 'cl-extra) nil)
(fsf/x () (require 'cus-face))
(require 'font-lock)

;;; (defun add-load-path (paths &optional at-end)
;;;   "Add the PATHS (after expand-file-name) to the load path, optionally at
;;; the end."
;;;  (let ((exp-list (if (listp paths)
;;;		      (map 'list (lambda (x) (expand-file-name x)) paths)
;;;		    (list (expand-file-name paths)))))
;;;    (setq load-path (if at-end
;;;			(append load-path exp-list)
;;;		      (append exp-list load-path)))))

(defun magus-jolly-candy-like-keyboard-quit ()
  (interactive)
  (signal 'quit
	  '("Don't touch it! It's the History Eraser Button, you fool!")))

(defun magus-bindings ()
  (global-set-key [f6] 'start-kbd-macro)
  (global-set-key [f7] 'end-kbd-macro)
  (global-set-key [f8] 'call-last-kbd-macro)
  (global-set-key "\C-\\" 'call-last-kbd-macro)
  (global-set-key [f9] 'name-last-kbd-macro)
  (global-set-key [f10] 'font-lock-mode)
  (global-set-key "\C-\M-c" 'compose-map)
  (global-set-key "\C-g" 'jolly-candy-like-keyboard-quit)
  (global-set-key "\M-g" 'goto-line)
  (global-set-key "\M-s" 'isearch-forward-regexp)
  (global-set-key "\M-r" 'isearch-backward-regexp)
  (global-set-key "\M-%" 'query-replace-regexp))

(defun magus-set-font (face fontspec) 
;;;(fsf/x
   (set-face-font face fontspec)
;;;   (set-face-property face 'font fontspec))
   )

(defun magus-set-foreground (face device color)
  (fsf/x 
   (if (eq device 'tty) nil 
     (set-face-foreground face color)) 
   (let ()
     (set-face-property face 'foreground 
			(make-color-specifier (cons device color))
			'prepend)
     (custom-set-faces '(face ((t (:foreground 'foreground)))))
     t
     )))

(defun magus-set-background (face device color) 
  (fsf/x 
   (if (eq device 'tty) nil 
     (set-face-background face color)) 
   (let* ()
     (set-face-property face 'background 
			(make-color-specifier (cons device color)) 
			'prepend)
     (custom-set-faces '(face ((t (:background 'background)))))
     )))
     

(defun* magus-face (face-name 
                    &key source 
                    &key font 
                    &key fg 
                    &key bg 
                    &key tty 
                    &key tty-bg)   
  (if source (copy-face source face-name) 
    (make-face face-name)) 
  (if (and font window-system) (magus-set-font face-name font) nil) 
  (if fg (magus-set-foreground face-name 'x fg) nil)
  (if bg (magus-set-background face-name 'x bg) nil) 
  (if tty (magus-set-foreground face-name 'tty tty) nil) 
  (if tty-bg (magus-set-background face-name 'tty tty-bg) nil) 
  (make-face face-name))

(defun* magus-face-shotgun (face-list
			    &key font
			    &key fg 
			    &key bg)
  (map 'list
       (lambda (x)
	 (progn
	   (if font (magus-set-font x font) nil)
	   (if fg (magus-set-foreground x 'x fg) nil)
	   (if bg (magus-set-background x 'x bg) nil)))
       face-list)
  )

(defun* magus-face-shotgun-tty (face-list
				&key fg 
				&key bg)
  (map 'list
       (lambda (x)
	 (progn
	   (if fg (magus-set-foreground x 'tty fg) nil)
	   (if bg (magus-set-background x 'tty bg) nil)))
       face-list)
  )


(defun magus-faces ()
  (when (or magus-in-xemacs
	    (eq window-system 'x))
    
    ;; Default FSF Emacs faces
    (fsf/x (set-default-font
	    "-schumacher-clean-medium-r-normal--12-*-*-*-*-*-iso8859-1")
	   nil)

    (magus-face 'default 
                :font "-*-clean-medium-r-*-*-12-*-*-*-*-*-iso8859-1" 
                :fg "white" :bg "black" :tty "brightwhite")
 
    (magus-face 'bold :font "-*-clean-bold-r-*-*-12-*-*-*-*-*-iso8859-1") 
    (magus-face 'italic :font "-*-clean-medium-i-*-*-12-*-*-*-*-*-iso8859-1") 
    (magus-face 'bold-italic :font "-*-clean-bold-r-*-*-12-*-*-*-*-*-iso8859-1") 
 
    ;; misc faces 
    (magus-face 'paren-match :source 'default :bg "grey30" :tty-bg "darkgrey") 
    (magus-face 'paren-mismatch :source 'default :bg "DeepPink" :tty-bg "magenta") 
 
    (magus-face 'modeline :tty "white" :tty-bg "darkgrey")
    (magus-face 'modeline-buffer-id :tty "brightblue")
    (magus-face 'modeline-mousable :tty "brightred")
    (magus-face 'modeline-mousable-minor-mode :tty "brightgreen")
 
    ;; why aren't tty colors set for these? 
    (magus-face 'blue :tty "brightblue") 
    (magus-face 'red :tty "brightred") 
    (magus-face 'green :tty "brightgreen") 
    (magus-face 'yellow :tty "brightyellow") 

    ;; zephyr faces 
    (magus-face 'zephyr-normal-face :source 'bold 
                :fg "green3" :tty "brightgreen") 
    (magus-face 'zephyr-andrew-face :source 'bold 
                :fg "MediumOrchid3" :tty "brightmagenta") 
    (magus-face 'zephyr-interesting-face :source 'bold 
                :fg "slateblue" :tty "blue") 
    (magus-face 'zephyr-personal-face :source 'default 
		:fg "firebrick" :tty "red") 
    (magus-face 'zephyr-sent-face :source 'bold 
                :fg "dodgerblue" :tty "brightcyan") 
    (magus-face 'zephyr-header-face :source 'bold)
    (magus-face 'zephyr-reminder-face :source 'default 
                :fg "grey50" :tty "darkgrey") 
    (magus-face 'zephyr-outgoing-face :source 'default 
                :fg "grey50" :tty "darkgrey") 
    (magus-face 'zephyr-mail-face :source 'bold 
                :fg "red" :tty "brightred") 
    (magus-face 'zephyr-warning-face :source 'bold
		:fg "orange" :tty "yellow")
    (magus-face 'zephyr-access-face :source 'bold
		:fg "orange1" :tty "brightyellow")
    (magus-face 'zephyr-userid-face :source 'italic 
                :fg "grey70" :tty "white") 
 
    ;; standard font-lock faces 
    (magus-face 'font-lock-comment-face :source 'default 
                :fg "orchid" :tty "brightmagenta") 
    (magus-face 'font-lock-doc-string-face :source 'default 
                :fg "green3" :tty "brightgreen")
    (magus-face 'font-lock-function-name-face :source 'default 
                :fg "red" :tty "brightred")
    (magus-face 'font-lock-keyword-face :source 'default 
                :fg "dodgerblue" :tty "brightblue")
    (magus-face 'font-lock-preprocessor-face :source 'default 
                :fg "grey50" :tty "darkgrey") 
    (magus-face 'font-lock-reference-face :source 'default 
                :fg "red3" :tty "brightred") 
    (magus-face 'font-lock-string-face :source 'default 
                :fg "green2" :tty "green")
    (magus-face 'font-lock-type-face :source 'default 
                :fg "gold" :tty "brightyellow")
    (magus-face 'font-lock-variable-name-face :source 'default 
                :fg "orange" :tty "yellow")
 
    ;; latex is "special" 
    (magus-face 'font-latex-bold-face :source 'bold 
                :fg "DarkOliveGreen" :tty "green") 
    (magus-face 'font-latex-italic-face :source 'italic 
                :fg "DarkOliveGreen" :tty "green") 
    (magus-face 'font-latex-math-face :source 'default 
                :fg "SaddleBrown" :tty "yellow") 
    (magus-face 'font-latex-sedate-face :source 'default 
                :fg "DimGray" :tty "darkgrey") 
    (magus-face 'font-latex-string-face :source 'font-lock-string-face) 
    (magus-face 'font-latex-warning-face :source 'bold 
                :fg "red" :tty "brightred") 
 
    ;;; face lists for shotgun face changing
    ;; basic faces
    (setq magus-basic-face-list '(default bold italic bold-italic))
    ;; misc faces 
    (setq magus-misc-face-list '(paren-match paren-mismatch
					     modeline modeline-buffer-id
					     modeline-mousable
					     modeline-mousable-minor-mode
					     blue red green yellow))
    ;; zephyr faces 
    (setq magus-zephyr-face-list '(zephyr-normal-face
				   zephyr-andrew-face
				   zephyr-interesting-face
				   zephyr-personal-face
				   zephyr-sent-face
				   zephyr-header-face
				   zephyr-reminder-face
				   zephyr-outgoing-face
				   zephyr-mail-face
				   zephyr-warning-face
				   zephyr-access-face
				   zephyr-userid-face))
    ;; standard font-lock faces 
    (setq magus-font-lock-face-list '(font-lock-comment-face
				      font-lock-doc-string-face
				      font-lock-function-name-face
				      font-lock-keyword-face
				      font-lock-preprocessor-face
				      font-lock-reference-face
				      font-lock-string-face
				      font-lock-type-face
				      font-lock-variable-name-face))
    ;; latex
    (setq magus-latex-face-list '(font-latex-bold-face
				  font-latex-italic-face
				  font-latex-math-face
				  font-latex-sedate-face
				  font-latex-string-face
				  font-latex-warning-face))

    (defun magus-face-white-on-black ()
      (interactive)
      (magus-face-shotgun magus-basic-face-list :bg "black" :fg "brightwhite")
      (magus-face-shotgun magus-zephyr-face-list :bg "black")
      (magus-face-shotgun magus-font-lock-face-list :bg "black")
      (magus-face-shotgun-tty magus-basic-face-list :bg "black" :fg "brightwhite")
      (magus-face-shotgun-tty magus-zephyr-face-list :bg "black")
      (magus-face-shotgun-tty magus-font-lock-face-list :bg "black"))
    
    (defun magus-face-black-on-white ()
      (interactive)
      (magus-face-shotgun magus-basic-face-list :bg "white" :fg "black")
      (magus-face-shotgun magus-zephyr-face-list :bg "white")
      (magus-face-shotgun magus-font-lock-face-list :bg "white")
      (magus-face-shotgun-tty magus-basic-face-list :bg "brightwhite" :fg "black")
      (magus-face-shotgun-tty magus-zephyr-face-list :bg "brightwhite")
      (magus-face-shotgun-tty magus-font-lock-face-list :bg "brightwhite"))
    
    t))

(provide 'magus-macs)

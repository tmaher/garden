;; Functions for handling zwgc-like @ formatting commands
;;
;; Michael Welsh Duggan <md5i@cs.cmu.edu> (6/4/1998)
;;
;; modified 8/21/2000 to handle justification
;; modified 8/22/2000 to suit Xemacs
;;
;; To handle zwgc-like @ formatting foo in your buffer, add
;;
;; (zephyr-add-hook 'zephyr-insert-hook-list 'zephyr-zwgc-format-message 
;;                  'zephyr-touch)
;;
;; to your .emacs after loading zephyr.el.
;;
;;
;; IMPORTANT!  In Xemacs, font-lock-always-fontify-immediately must be
;; set to t in a zephyr buffer if font-lock and zephyr-zwgc are both
;; to be used.  You can use the following code to do this:
;;  
;; (add-hook 'zephyr-mode-hook
;;	     (lambda ()
;;	       (make-local-variable 'font-lock-always-fontify-immediately)
;;	       (setq font-lock-always-fontify-immediately t)))
;;


;;;  Test code

;(defun zwgc-test (start end)
;  (interactive "r")
;  (zephyr-zwgc-fontify-region start end))
;(defun zwgc-test-2 (start end)
;   (interactive "r")
;  (zephyr-zwgc-justify-region start end))

(defvar zephyr-zwgc-style-strip nil
  "*If set to non-nil, it will only strip the formatting, instead of using it."
)

(defun zephyr-zwgc-fontify-string (string)
  "Fontify STRING using zwgc `@' processing."
  (save-excursion
    (let ((buffer (generate-new-buffer " *zwgc-work*")))
      (set-buffer buffer)
      (buffer-disable-undo)
      (insert string)
      (zephyr-zwgc-fontify-region (point-min) (point-max))
      (prog1 (buffer-string)
	(kill-buffer buffer)))))

(defun zephyr-zwgc-format-message (msg)
  "Fontify a zephyr-gram MSG from zephyr.el using zwgc `@' processing."
  (let ((start (cdr (assq 'body-begin msg)))
	(end (cdr (assq 'body-end msg))))
    (save-restriction
      (narrow-to-region start end)
      (let (before-change-functions after-change-functions)
	(zephyr-zwgc-fontify-region start end)
	(zephyr-zwgc-justify-region (point-min) (point-max)))
      (setcdr (assq 'body-end msg) (point-max))))
  msg)

(if (not (fboundp 'color-name))
    (if (fboundp 'color-instance-name)
	(defun color-name (spec)
	  "Xemacs version of color-name for color specifiers."
	  (color-instance-name (specifier-instance spec)))
      (defalias 'color-name 'identity)))
(if (not (fboundp 'face-italic-p))
    (if (fboundp 'face-property-instance)
	(defun face-italic-p (face)
	  "Xemacs version of italic-p."
	  (let ((slant (cdr (assoc 'SLANT 
				    (font-instance-properties 
				     (face-property-instance face 'font))))))
	    (and (stringp slant) 
		 (or (string= (downcase slant) "o") 
		     (string= (downcase slant) "i")))))
      (defun face-italic-p (face) nil)))
(if (not (fboundp 'face-bold-p))
    (if (fboundp 'face-property-instance)
	(defun face-bold-p (face)
	  "Xemacs version of bold-p."
	  (let ((weight (cdr (assoc 'WEIGHT_NAME
				    (font-instance-properties 
				     (face-property-instance face 'font))))))
	    (and (stringp weight)
		 (string-match "bold\\|black" (downcase weight)))))
      (defun face-bold-p (face) nil)))
    

(defun zephyr-zwgc-make-face-name (face foreground background stipple
					bold-p italic-p underline-p)
  "Creates a unique NAME for any given face."
  (setq face (or face 'default))
  (concat (or foreground (color-name (face-foreground face))) "-"
	  (or background (color-name (face-background face))) "-"
	  (if (fboundp 'face-stipple)
	      (or stipple (face-stipple face)) "-")
	  (if (or (eq underline-p t)
		  (and (not (null underline-p)) (face-underline-p face)))
	      "u" "n")
	  "-"
	  (if (or (eq italic-p t)
		  (and (not (null italic-p)) (face-italic-p face)))
	      "i" "n")
	  "-"
	  (if (or (eq bold-p t)
		  (and (not (null bold-p)) (face-bold-p face)))
	      "b" "n")))

(or (fboundp 'modify-face)
    (defun modify-face (face foreground background stipple
			     bold-p italic-p underline-p)
      "`modify-face' for Xemacs."
      (if foreground
	  (condition-case nil
	      (set-face-foreground face foreground)
	    (error nil)))
      (if background
	  (condition-case nil
	      (set-face-background face background)
	  (error nil)))
      (if stipple
	  (condition-case nil
	      (set-face-stipple face stipple)
	    (error nil)))
      (condition-case nil
	  (cond ((null bold-p)
		 (make-face-unbold face))
		((eq bold-p t)
		 (make-face-bold face)))
	(error nil))
      (condition-case nil
	  (cond ((null italic-p)
		 (make-face-unitalic face))
		((eq italic-p t)
		 (make-face-italic face)))
	(error nil))
      (if (or (null underline-p) (eq underline-p t))
	  (condition-case nil
	      (set-face-underline-p face underline-p)
	    (error nil)))))

(defun zephyr-zwgc-make-arbitrary-face (face foreground background stipple
					     bold-p italic-p underline-p)
  "Makes an arbitrary face.
The new face is a copy of FACE, with FORE, BACK, BOLD, ITALIC, UNDERLINE,
STIPPLE, and FONT being the differences of the new face from the original.
Any of these values can be nil, meaning no difference.  If FACE is nil,
the `default' face is used."
  (let ((name (intern (concat "zephyr-face-"
			      (zephyr-zwgc-make-face-name
			       face foreground background stipple
			       bold-p italic-p underline-p)))))
    (if (facep name)
	name
      (copy-face (or face 'default) name)
      (modify-face name
		   foreground background stipple bold-p italic-p underline-p)
      name)))

(defvar zephyr-zwgc-matched-parens-alist
  '((?\( . ?\))
    (?\[ . ?\])
    (?{ . ?})
    (?< . ?>))
  "An alist of valid formatting command bracket pairs.")

(defalias 'zephyr-zwgc-not-implemented 'zephyr-zwgc-fontify-region)

(defvar zephyr-zwgc-formatting-command-alist
  '(("roman" . zephyr-zwgc-roman)
    ("b" . zephyr-zwgc-bold)
    ("bold" . zephyr-zwgc-bold)
    ("i" . zephyr-zwgc-italic)
    ("italic" . zephyr-zwgc-italic)
    ("l" . zephyr-zwgc-left-region)
    ("left" . zephyr-zwgc-left-region)
    ("c" . zephyr-zwgc-center-region)
    ("center" . zephyr-zwgc-center-region)
    ("r" . zephyr-zwgc-right-region)
    ("right" . zephyr-zwgc-right-region)
    ("large" . zephyr-zwgc-not-implemented) ; Not implemented
    ("medium" . zephyr-zwgc-not-implemented) ; Not implemented
    ("small" . zephyr-zwgc-not-implemented) ; Not implemented
    ("beep" . zephyr-zwgc-beep)
    ("font" . zephyr-zwgc-not-implemented) ; Not implemented
    ("color" . zephyr-zwgc-color))
  "The alist of supported @ commands for `zephyr-zwgc-fontify-region'.
The function should take a start, end, and property list, and be
prepared to deal with everything within the environment.")

(defun zephyr-zwgc-roman (start end props)
  (zephyr-zwgc-fontify-region start end (plist-put props 'face nil)))

(defun zephyr-zwgc-left-region (start end props)
  (zephyr-zwgc-fontify-region 
   start end (plist-put props 'justify 'left)))

(defun zephyr-zwgc-right-region (start end props)
  (zephyr-zwgc-fontify-region 
   start end (plist-put props 'justify 'right)))

(defun zephyr-zwgc-center-region (start end props)
  (zephyr-zwgc-fontify-region 
   start end (plist-put props 'justify 'center)))

(defun zephyr-zwgc-bold (start end props)
  (zephyr-zwgc-fontify-region
   start end (if zephyr-zwgc-style-strip props
	       (plist-put props 'face
			  (zephyr-zwgc-make-arbitrary-face
			   (plist-get props 'face)
			   nil nil nil t 'ignore 'ignore)))))

(defun zephyr-zwgc-italic (start end props)
  (zephyr-zwgc-fontify-region
   start end (if zephyr-zwgc-style-strip props
	       (plist-put props 'face
			  (zephyr-zwgc-make-arbitrary-face
			   (plist-get props 'face)
			   nil nil nil 'ignore t 'ignore)))))

(defun zephyr-zwgc-beep (start end props)
  (or zephyr-zwgc-style-strip (ding t))
  (zephyr-zwgc-fontify-region start end props))

(defun zephyr-zwgc-font (start end props)
  (delete-region start end)
  (throw 'zephyr-zwgc-face-change props))

(defun zephyr-zwgc-color (start end props)
  (let ((newface (if zephyr-zwgc-style-strip props
		   (zephyr-zwgc-make-arbitrary-face
		    (plist-get props 'face)
		    (buffer-substring start end) nil nil 'ignore 'ignore
		    'ignore))))
    (delete-region start end)
    (throw 'zephyr-zwgc-face-change (plist-put props 'face newface))))

(defun zephyr-zwgc-find-matching-close-paren (paren)
  "Finds the mate to the given PAREN.
Assumes that you are one character deep into the nesting.  Does its
work without the benefit of the syntax tables."
  (let* ((count 1)
	 (matching (cdr (assoc paren zephyr-zwgc-matched-parens-alist)))
	 (regex (concat (regexp-quote (char-to-string paren)) "\\|"
			(regexp-quote (char-to-string matching)))))
    (while (and (> count 0) (re-search-forward regex nil t))
      (cond ((= (preceding-char) paren)
	     (setq count (1+ count)))
	    ((= (preceding-char) matching)
	     (setq count (1- count)))))
    (zerop count)))

(defun zephyr-zwgc-fontify-region (start end &optional proplist)
  "Fontifies a region using the zwgc `@' processing syntax.
PROPLIST is the default set of text properties for the region."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (let ((case-fold-search t)
	    (props (copy-sequence proplist)))
	(or (plist-get props 'face) (setq props (plist-put props 'face nil)))
	(or (plist-get props 'justify) 
	    (setq props (plist-put props 'justify 'left)))
	(setq props (plist-put props 'start-open t))
	(setq props (plist-put props 'end-open t))
	(setq proplist (copy-sequence props))
	(goto-char (point-min))
	(while (search-forward "@" nil t)
	  (add-text-properties start (point) props)
	  (cond ((looking-at "@")
		 (delete-char 1))
		((looking-at "\\([a-z0-9_]*\\)[([{<]")
		 (save-match-data
		   (let* ((cmd (assoc (downcase (match-string 1))
				      zephyr-zwgc-formatting-command-alist))
			  (format-func (if cmd (cdr cmd)
					 'zephyr-zwgc-fontify-region))
			  (start (match-end 0)))
		     (setq props
			   (catch 'zephyr-zwgc-face-change
			     (goto-char start)
			     (funcall 
			      format-func start
			      (if (zephyr-zwgc-find-matching-close-paren
				   (char-after (1- start)))
				  (progn
				    (delete-backward-char 1)
				    (point))
				(goto-char (point-max))) props)
			     (copy-sequence proplist)))
		     (setq proplist (copy-sequence props))))
		 (delete-region (1- (match-beginning 0)) (match-end 0))))
	  (setq start (point)))
	(add-text-properties start (point-max) props)))))

(if (not (fboundp 'line-end-position))
    (defun line-end-position ()
      (save-excursion (end-of-line) (point))))

(defun zephyr-zwgc-justify-region (start end)
  "Justifies a region in the same manner as zwgc.
The text in the region should have the text property `justify' set to
`left', `right', or `center' as necessary.  Adding this text property
is usually handled by `zephyr-zwgc-fontify-region'."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (let ((size fill-column))
	(while (not (eobp))
	  (let ((start (point))
		(limit (line-end-position))
		(left 0) (right 0) (center 0))
	    (while (< start limit)
	      (let ((pos 
		     (next-single-property-change start 'justify nil limit)))
		(let ((current (get-text-property start 'justify)))
		  (set current (+ (eval current) (- pos start))))
		(setq start pos)))
	    (setq size (max size (if (zerop center) (+ left right)
				   (+ (* 2 (max left right)) center))))
	    (forward-line)))
	(goto-char (point-min))
	(while (not (eobp))
	  (let ((start (point))
		(limit (line-end-position))
		(left "") (right "") (center ""))
	    (while (< start limit)
	      (let ((pos 
		     (next-single-property-change start 'justify nil limit)))
		(let ((current (get-text-property start 'justify)))
		  (set current (concat (eval current) 
				       (buffer-substring start pos))))
		(setq start pos)))
	    (delete-region (point) limit)
	    (insert left)
	    (let ((ll (length left))
		  (lc (length center))
		  (lr (length right)))
	      (if (or (/= 0 lr) (/= 0 lc))
		(progn
		  (if (zerop lc)
		      (progn
			(insert (make-string (- size (+ ll lr)) 32))
			(insert right))
		    (insert (make-string (- (/ (- size lc) 2) ll) 32))
		    (insert center)
		    (if (/= 0 lr)
			(progn
			  (insert 
			   (make-string (- (/ (1+ (- size lc)) 2) lr) 32))
			  (insert right)))))))
	    (forward-line)))))))

(provide 'zephyr-zwgc)

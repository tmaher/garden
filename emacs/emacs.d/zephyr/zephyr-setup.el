;;; Emacs Zephyr Setup File

;;; This file serves as a wrapper for the elisp zephyr libraries provided by
;;; the CMU CS Dept.  It is chiefly derived from source provided by
;;; Nat Lanza <magus@cs.cmu.edu>, with modification done by
;;; Tom Maher <tardis@ece.cmu.edu>.

;;; This file is known to work with XEmacs 20, 21, and GNU Emacs 19, 20.

;;; Zephyr requirements
(require 'zephyr)
(require 'zlocate-finger)
(require 'zephyr-logging)
(require 'zephyr-style-strip)
(require 'magus-macs)

(if (fboundp 'fsf/x) nil
  (progn
    (if (boundp 'user-in-xemacs) nil
      (defvar user-in-xemacs (if (string-match "XEmacs" emacs-version) 
				 t nil)))
    (defmacro fsf/x (fsf x) `(if user-in-xemacs ,x ,fsf)))
  )

(defun zephyr-setup-setqs ()
  "Set up various variables"
  (setq
   tzc-binary "tzc"
   zephyr-unauth-string "!!!"
   zephyr-buffer-limit nil
   zephyr-log-buffer-limit 50000
   zephyr-beep-personal-only t
   zephyr-deiconify-on-receive t
   zephyr-lazy-beep-time 1
   zephyr-send-function 'zephyr-send-show-multiple
   zephyr-opcodes-ignore-list t
   zephyr-warn-empty-message t
   zephyr-send-divider "<<< \n"
   zephyr-send-divider-regexp "<<< *\n?"
   zephyr-failed-string "[FORGED]"
   zephyr-exposure "NET-ANNOUNCED"

   zephyr-realm-aliases 
   '(("CS" . "CS.CMU.EDU")
     ("ECE" . "ECE.CMU.EDU")
     ("ANDREW" . "ANDREW.CMU.EDU")
     ("DEMENTIA" . "DEMENTIA.ORG")
     ("SNURGLE" . "SNURGLE.ORG")
     ("THEKEEP" . "THEKEEP.ORG")
     ("WATSON" . "WATSON.ORG")
     ("ATHENA" . "ATHENA.MIT.EDU"))

   zephyr-font-lock-keywords
   '(("^[^\n«»]+<<<" . zephyr-outgoing-face)
     ("^[a-zA-Z0-9_.@]+(MAIL:.*)!*» \\[.*\\]$" . zephyr-mail-face)
     ("^[a-zA-Z0-9_.@]+(LOGIN:)!*» \\[.*\\]$" . zephyr-znol-face)
     ("^[a-zA-Z0-9_.@]+!*» \\[.*\\]$" . zephyr-personal-face)
     ("^[a-zA-Z0-9_.@]+(.*@ANDREW)!*» \\[.*\\]$" . zephyr-andrew-face)
     ("^[a-zA-Z0-9_.@]+(.*@CS)!*» \\[.*\\]$" . zephyr-cs-face)
     ("^[a-zA-Z0-9_.@]+(.*@ECE)!*» \\[.*\\]$" . zephyr-ece-face)
     ("^[a-zA-Z0-9_.@]+(.*@[A-Z.]+)!*» \\[.*\\]$" . zephyr-foreign-face)
     ("^[a-zA-Z0-9_.@]+(.*)!*» \\[.*\\]$" . zephyr-native-face)
     )

   zephyr-hook-list
   '(zephyr-parse
     zephyr-dispatch
     zephyr-setup-parse-fields
     zephyr-setup-initial-add-banner
     zephyr-style-strip-signature
     zephyr-style-strip-opcode
     zephyr-setup-final-add-banner
     zephyr-munge-cyrus
     zephyr-insert)

  zephyr-insert-hook-list
   '(zephyr-ping-notify
     zephyr-insert-message
     zephyr-make-message-extents
     zephyr-touch
     zephyr-notify-with-message
     zephyr-notify-with-deiconify
     zephyr-notify-with-beep
     zephyr-history-hook)
   ))

(defun zephyr-setup-init-faces ()
  "Initializes the zephyr font faces."

  (require 'font-lock)

  (if (boundp 'zephyr-additional-faces) nil 
    (defvar zephyr-additional-faces nil))

  (setq zephyr-faces
	(append 
	 '((zephyr-native-face "cyan")
	   (zephyr-foreign-face "magenta")
	   (zephyr-andrew-face "green")
	   (zephyr-cs-face "orange")
	   (zephyr-ece-face "cyan")
	   (zephyr-personal-face "red")
	   (zephyr-mail-face "red")
	   (zephyr-outgoing-face "dodgerblue")
	   (zephyr-znol-face "darkgrey"))
	 zephyr-additional-faces))

  (setq-default font-lock-maximum-decoration t)
  ;; (add-hook 'font-lock-mode-hook 'turn-on-lazy-lock)

  (maplist '(lambda (x) 
	      (let ((face (car (car x)))
		    (fgcolor (cdr (car x))))
		(make-face face)
		(set face face)
		(set-face-foreground face (car fgcolor))
		))
	   zephyr-faces)
  )

;;; give names to the various fields and add them to the alist.  also
;;; add the print-as tag, containing the printed rep.
;;; we convert the class to uppercase, since case is supposed to be
;;; insignificant in class names.
(defun zephyr-setup-parse-fields (msg)
  (let ((class (intern (upcase (zephyr-make-string (cdr (assq 'class msg))))))
	(urgent (string-equal (downcase (cdr (assq 'instance msg))) "urgent"))
	(sender (cdr (assq 'sender msg)))
	(opcode (intern (zephyr-make-string (cdr (assq 'opcode msg)))))
	(fields (cdr (assq 'message msg)))
	(time-secs  (cdr (assq 'time-secs msg))))
    ;;;; this is for a kludge to let emacs18 know (sort of) what time it is
    (if time-secs
	(setq zephyr-last-zgram-timestamp time-secs))
    (cond 
     ((eq class 'LOGIN)
      (let ((host (nth 0 fields))
	    (when (nth 1 fields))
	    (tty (nth 2 fields))
	    (type (cond ((eq opcode 'USER_LOGIN) "login")
			((eq opcode 'USER_LOGOUT) "logout")
			(t "<weird LOGIN opcode>"))))
	(append (list (cons 'print-as (concat ""))) msg)))
     (;; in messages, the first field is a signature, and the
      ;; second is the message body.
      ;; (We'll treat unknown classes like class 'MESSAGE)
      t 
      (let* ((len (length fields))
	     (sig (cond ((or (> len 1)
			     (and (> len 0) (not urgent)))
			 ;; strip trailing newline (multi-line sigs 
			 ;; will be truncated)
			 (string-match "\\(.*\\)\n*"
				       (car fields))
			 (zephyr-match (car fields) 1))
			(t sender)))
	     (body (cond ((eq opcode 'PING) "<PING>")
			 ;; urgent zgrams from server have no sig
			 ((and urgent (= 1 len))
			  (car fields))
			 ((= 1 len) "") ; usually just means empty body
;;; zwgc just ignores extra fields, so we will too
;;;			      ((not (= len 2)) 
;;;			       (format "malformed message: %s" fields))
			 (t (cadr fields))
			 )))
	(append (list (cons 'print-as body)
		      (cons 'signature sig)
		      (cons 'body body))
		msg)))
     )))

(defun zephyr-setup-fromline (msg)
  "return the from-line for MSG.  e.g. \"susan(graffiti)\""
  (let* ((inst  (zephyr-alias-realm (cdr (assq 'instance msg))))
	 (class (zephyr-make-string (cdr (assq 'class msg))))
	 (recip (zephyr-alias-realm (cdr (assq 'recipient msg))))
	 (sender (zephyr-alias-realm (cdr (assq 'sender msg))))
	 (inst-realm (if (and (> (length recip) 0)
			      (eq (aref recip 0) ?@))
			 (concat (zephyr-alias-realm inst) recip)
		       (zephyr-alias-realm inst))))
    (if (and (string-equal (upcase (zephyr-alias-realm inst)) "PERSONAL")
	     (string-equal (upcase class) "MESSAGE")
	     (zephyr-personal-msg-p msg))
	sender
      (concat 
       sender "("
       (if (string= class "LOGIN")
	   (zephyr-class-instance-string class "")
	 (zephyr-class-instance-string class inst-realm))
       ")"))))
;;;	       class (if (string= class "LOGIN") " " inst-realm) ")")))))
  
;;; add the string used to "introduce" a message.
(defun zephyr-setup-initial-add-banner (msg)
  (let ((auth (cdr (assq 'auth msg)))
	(class (cdr (assq 'auth msg))))
    (cons 
     (cons 
      'banner
      (concat (zephyr-setup-fromline msg)
	      (cond ((eq auth 'yes) "")
		    ((eq auth 'failed) zephyr-failed-string)
		    ((eq auth 'no) zephyr-unauth-string))
	      zephyr-receive-divider))
     msg)))

(defun zephyr-munge-cyrus (msg)
  "Munge zephyrgrams from Cyrus into a more pleasing and shorter form."
  (if (and (eq (cdr (assq 'class msg)) 'MAIL)
	   (string-match "^You have new mail" (cdr (assq 'print-as msg))))
      (let* ((text (cdr (assq 'print-as msg))))
	(string-match "^From: \\([^\n]*\\)" text)
	(setq sender (match-string 1 text))
	(string-match "^Subject: \\([^\n]*\\)" text)
	(setq subj (match-string 1 text))
	(setcdr 
	 (assq 'print-as msg)
	 (concat
	  (if (> (length sender) 38) (substring sender 0 38) sender) " : "
	  (if (> (length subj) 38) (substring subj 0 38) subj) ""))))
  msg)

(defun zephyr-setup-final-add-banner (msg)
  (let* ((opc (assq 'opcode msg))
	 (opcode (and (cdr opc) (symbol-name (cdr opc))))
	 (fromhost (cdr (assq 'fromhost msg)))
	 (class (cdr (assq 'class msg)))
	 (signature (cdr (assq 'signature msg)))
	 (instance (zephyr-alias-realm (cdr (assq 'instance msg)))))
    (let ((banner (assq 'banner msg))
	  (timestmp (cdr (assq 'time msg)))
	  (curl 0))
      (if banner
	  (setcdr 
	   banner
	   (concat 
	    (progn (setq curl (+ curl (length (cdr banner))))
		   (cdr banner))
;;;	    (if (> curl 68)
;;;		(progn (setq curl 0) "\n")
;;;	      "")
	    (progn (setq curl (+ curl 9))
		   (concat "[" (substring timestmp 11 19)))
;;;	    (if (> (+ curl (length signature) 1) 79)
;;;		(progn (setq curl 0) "\n")
;;;	      (progn (setq curl (1+ curl)) " "))
	    (progn (setq curl (1+ curl)) " ")
	    (if (string= class "LOGIN")
		(concat 
		 (if (> (+ curl (length opcode) 3) 79)
		     (progn (setq curl 0) "\n")		   
		   ;;		   (progn (setq curl (1+ curl)) " ")
		   )
		 (downcase fromhost) " "
		 (progn (setq curl (+ 2 (length opcode) curl))
			(if (string-match "LOGIN" opcode) "LOGIN" "LOGOUT"))
		 "]")
	      (concat 
	       (progn (setq curl (+ curl (length signature)))
		      (concat "" signature))
	       (if (string-match "nil" opcode) ()
		 (concat
;;;		  (if (> (+ curl (length opcode) 3) 79)
;;;		      (progn (setq curl 0) "\n")
;;;		    (progn (setq curl (1+ curl)) " "))
		  "::"
		  (progn (setq curl (+ 2 (length opcode) curl)) opcode)))
	       "]\n")
;;;	       (if (> curl 76) (concat "]\n")
;;;		 (concat "]\n"))
	      ))))))
  msg)

(defvar zephyr-use-ssh nil
  "*Set to t in order to ssh to another machine and run a tzc there.
You'll also want to set zephyr-ssh-host (the remote host), zephyr-ssh-userid
(username on that host), and  zephyr-ssh-tzc (path to the tzc execuable), 
each one a simple string.  Additionally, sshd on the remote host MUST grant
a kerb tgt on login, or tzc will fail miserably.")

(defvar zephyr-use-random-sig nil
  "*Set this to t, and set zephyr-signatures (note plural) to a list of 
strings, and a different sig will be sent at random with each zgram.")

(defun zephyr-random-sig (a b)
  "Hook to randomize outgoing zephyr signature.
   Courtesy Zach Loafman <zml@cmu.edu>"
  (if zephyr-use-random-sig 
      (setq zephyr-signature
	    (nth (random (length zephyr-signatures)) zephyr-signatures))))

(defun zephyr-other-frame () (interactive)
  (select-frame (make-frame))
  (zephyr-new-buffer))

(defun zephyr-personal ()
  "start a buffer for personal zephyrs."
  (interactive)
  (if (string= (car zephyr-initial-history) "nobody")
      (setq zephyr-initial-history
	    (list (if (string= zephyr-id nil) (user-login-name)
		    zephyr-id))))
  (zephyr-new-buffer "personal-zgrams")
  (setq zephyr-filters
	(append zephyr-filters
		(list (list (cons 'recipient
				  (concat "^" (eval 'zephyr-id)))
			    t)
		      '(nil)))))

;;; Bunch of code from Zach.

(setq zephyr-who ())
(setq zephyr-signatures ())

(defvar zephyr-use-anyone t
  "*Use your .anyone file to track login/logout subscriptions instead of
having to set them yourself using zephyr-extra-subscriptions.

Actually, the contents of your .anyone gets read in and appended to
zephyr-extra-subscriptions.")

(defvar zephyr-use-zephyr-subs nil
  "*Set your subscriptions using your .zephyr.subs.tzc file instead of having
to set them using zephyr-extra-subscriptions.")

(setq anyone-file "~/.anyone")
(setq signatures-file "~/.zsigs")

(defun zephyr-anyone ()
  (save-excursion
    (let ((tmpbuf (get-buffer-create "*anyone-temp*")))
      (set-buffer tmpbuf)
      (erase-buffer) 
      (insert-file-contents anyone-file)
      (goto-char (point-min))
      (setq beg (point-min))
      (while (< (forward-line 1) 1)
	(setq tmp (buffer-substring beg (- (point) 1)))
	(setq zephyr-who (append zephyr-who (list tmp)))
	(let ((match (string-match "\@" tmp)))
	  (if match
	      (let ((realm (concat "*" (substring tmp match))))
		(setq zephyr-extra-subscriptions 
		      (append zephyr-extra-subscriptions
			      (list (list "LOGIN" tmp realm)))))
	    (setq 
	     zephyr-extra-subscriptions
	     (append zephyr-extra-subscriptions 
		     (list 
		      (list "LOGIN" (concat tmp "@ANDREW.CMU.EDU") "*"))))))
	(setq beg (point)))
      (kill-buffer tmpbuf))))

(defun zephyr-zsigs ()
  (save-excursion
    (let ((tmpbuf (get-buffer-create "*zsig-temp*")))
      (set-buffer tmpbuf)
      (erase-buffer) 
      (insert-file-contents signatures-file)
      (goto-char (point-min))
      (setq beg (point-min))
      (while (< (forward-line 1) 1)
	(setq tmp (buffer-substring beg (- (point) 1)))
	(setq zephyr-signatures (append zephyr-signatures (list tmp)))
	(setq beg (point)))
      (kill-buffer tmpbuf))))

;;;(defun zwatch-start ()
;;;  (interactive)
;;;  (setq zwatch-buf (get-buffer-create "*zwatch*"))
;;;  (zwatch-tickle))
;;;
;;;(setq znol-program 
;;;      (cond ((file-exists-p "/usr/contributed/lib/tzc/zl-e")
;;;	     "/usr/contributed/lib/tzc/zl-e")
;;;	    (t "zl-e")))
;;;
;;;(defun zwatch-tickle ()
;;;  (interactive)
;;;  (save-excursion
;;;    (set-buffer zwatch-buf)
;;;    (erase-buffer)
;;;    (start-process "znol" zwatch-buf znol-program "")))
;;;
;;;(defun zwatch-tickle-hook (msg)
;;;  (let ((class (cdr (assq 'class msg))))
;;;    (if (string= class "LOGIN")
;;;	(zwatch-tickle)))
;;;  msg)

(defun zephyr-history-hook (msg)
  (setq zephyr-class-history 
	(cons (cdr (assq 'class msg)) zephyr-class-history))
  (setq zephyr-instance-history 
	(cons (cdr (assq 'instance msg)) zephyr-instance-history))
  (setq zephyr-recipient-history 
	(cons (cdr (assq 'recipient msg)) zephyr-recipient-history))
  msg)

(defun zml-format-regex (var)
  (format "^%s" (regexp-quote (format "%s" var))))

(defun zephyr-ignore-last ()
  (interactive)
  (let* ((class (car zephyr-class-history))
	 (instance (car zephyr-instance-history))
	 (recip (car zephyr-recipient-history))
	 (class-str (zml-format-regex class))
	 (instance-str (zml-format-regex instance))
	 (recip-str (zml-format-regex recip)))
    (if (y-or-n-p (format "Really ignore (%s:%s)? " class instance))
	(progn
	  (setq zephyr-filters
		(append zephyr-filters
			(list (list 
			       (cons `class class-str)
			       (cons `instance instance-str)
			       (cons `recipient recip-str)
			       nil))))
	  (message (format "(%s:%s) ignored." class instance)))
      (message "NOT ignored.")))
  t)

(defun zephyr-ignore-last-class ()
  (interactive)
  (let* ((class (car zephyr-class-history))
	 (instance (car zephyr-instance-history))
	 (recip (car zephyr-recipient-history))
	 (class-str (zml-format-regex class))
	 (instance-str (zml-format-regex instance))
	 (recip-str (zml-format-regex recip)))
    (if (y-or-n-p (format "Really ignore class %s? " class))
	(progn
	  (setq zephyr-filters
		(append zephyr-filters
			(list (list 
			       (cons `class class-str)
			       (cons `recipient recip-str)
			       nil))))
	  (message (format "class %s ignored." class)))
      (message "NOT ignored.")))
  t)

(defvar zephyr-class-history ())
(defvar zephyr-instance-history ())
(defvar zephyr-recipient-history ())

;;; end of most of Zach's code

(defun zephyr-public ()
  "start a buffer for public zephyrs."
  (interactive)
  (if (string= (car zephyr-initial-history) "nobody") 
      (setq zephyr-initial-history (list "(test)")))
  (zephyr-new-buffer "public-zgrams")
  (setq zephyr-filters
	(append (list (list 
		       (cons 'recipient (concat "^" (eval 'zephyr-id)))
		       nil)) zephyr-filters)))

(defun zephyr-dual ()
  "starts two buffers, one public, one private."
  (interactive)
  (zephyr-public)
  (split-window)
  (other-window 1)
  (zephyr-personal)
  t)

;;; (defun zephyr-dual-with-zwatch ()
;;;  "starts two zgram buffers; one public, one private, and one for zwatch."
;;;  (interactive)
;;;
;;;  (if (member 'zwatch-tickle-hook zephyr-hook-list) zephyr-hook-list
;;;    (setq zephyr-hook-list (append zephyr-hook-list
;;;				   (list 'zwatch-tickle-hook))))
;;;
;;;  (zephyr-personal)
;;;  (zwatch-start)
;;;  (let* ((w (selected-window)))
;;;    (let* ((w2 (split-window w (- (window-width) 25) t)))
;;;      (set-window-buffer w2 "*zwatch*")))
;;;  
;;;  (split-window)
;;;  (other-window 1)
;;;  (zephyr-public)
;;;  (other-window 2)
;;;  
;;;  (local-set-key "\C-c\C-z" `zwatch-tickle)
;;;  (local-set-key "\C-c\C-i" `zephyr-ignore-last)
;;;  (local-set-key "\C-c\C-o" `zephyr-ignore-last-class)
;;;  (local-set-key "\C-c\C-y" `zephyr-quote-message)
;;;  t)

(defun do-zephyr-setup ()

  (if (< max-lisp-eval-depth 500)
      (setq max-lisp-eval-depth 500))

  (zephyr-setup-init-faces)
  (zephyr-setup-setqs)

  (add-hook 
   'zephyr-mode-hook
   '(lambda ()
      (auto-fill-mode 1)
      (setq fill-column 78)
      (make-variable-buffer-local 'blink-matching-paren)
      (setq blink-matching-paren t)
      (fsf/x
       (if (or (eq zephyr-running-fsf23 t)
               (eq window-system 'x))
           (font-lock-add-keywords nil zephyr-font-lock-keywords))
       (progn (setq font-lock-keywords zephyr-font-lock-keywords)
              (font-lock-mode)))
      (local-set-key "\C-c\C-f" 'zephyr-zlocate-finger)
      ))

  (add-hook 'zephyr-before-send-hook 'zephyr-random-sig)

  (load "~/.zephyr-options")
  (if zephyr-use-ssh (load "zephyr-ssh"))
  (setq zephyr-receive-program (append 
  				(list tzc-binary "-e" zephyr-exposure)
  				(if zephyr-use-zephyr-subs (list "-s"))))
  (if zephyr-use-anyone (zephyr-anyone))
  )

(do-zephyr-setup)

(provide 'zephyr-setup)

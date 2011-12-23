;;
;; This package allows you to run zephyr-mode using a tzc process running
;; on a remote host. You might wish to do this if your local workstation is
;; an architecture for which a local tzc client is not available. 
;;
;; - Arup Mukherjee 11/93
;;
;; Note: make sure your zephyr-remote-death-time is set appropriately. If it
;; is too low, you'll wind up creating lots of detached sessions on the
;; machine that is running your zephyr-receive program. 
;;
;; CHANGES:
;;  14 Jan 1997 - introduced zephyr-remote-keepalive-interval

(require 'zephyr)

(defvar zephyr-remote-host "XXXX.YYYY.cs.cmu.edu"
  "*Host on which to make the remote connection")

(defvar zephyr-remote-userid (user-login-name)
  "Userid to use when logging in to the remote host")

(defvar zephyr-remote-passwd nil
  "Password to use when logging in to the remote host, or nil 
   to cause the program to prompt you for it")

(defvar zephyr-remote-retain-passwd nil
  "Whether or not to remember the password on the remote host once it has 
   been specified.")

(defvar zephyr-remote-login-regexp "login:"
  "Regexp matching the login prompt")

(defvar zephyr-remote-passwd-regexp "Password:"
  "Regexp matching the password prompt")

(defvar zephyr-remote-reconnect-or-ready-regexp 
  "\\(\\[yes\\]  \\)\\|\\(; tzc\\)"
  "Regexp that will be matched indicating either that tzc has started, 
   on that Mach is asking whether to reconnect to a detached tty.")

(defvar zephyr-remote-reconnect-regexp "Reattach \\? \\[yes\\]  "
  "Regexp that will be matched at a when Mach is asking whether or not 
   to reconnect to detached sessions. Should correspond to half of the 
   zephyr-remote-reconnect-or-ready-regexp.")

(defvar zephyr-remote-ready-regexp "; tzc"
  "Regexp that will be matched indicating that the zephyr-receive-program
   has started and is ready for input")

(defvar zephyr-remote-tzc-wrapper-program
  "/afs/cs/misc/others/all_mach/omega/etc/remote_tzc_start"
  "A command to execute on the remote host, which will start up the zephyr 
   receiver program. Cannot take any arguments.")    

(defvar zephyr-remote-death-time 5
  "How long, in seconds, to wait to make sure the remote zephyr receiver 
   process has had a chance to die. If this value is too low, you wind 
   up creating detached sessions at the remote host")

(defvar zephyr-remote-keepalive-interval nil
  "*If non-nil, emacs will periodically prod tzc (with the given period,
in seconds).  This may help to prevent the telnet connection from dying.
Changes to this variable don't take effect until the receiver process 
is (re)started. 

Warning: this feature requires tzc version 2.5 or later and 
         zephyr.el version 2.4.7 or later.
")

(defun zephyr-remote-wait-for-prompt (regexp)
  (message"waiting for prompt \"%s\"" regexp)
  (let ((orig-pt (point)))
    (while (not (re-search-forward regexp nil t))
      (accept-process-output)
      (goto-char orig-pt)))
  (message "waiting for prompt \"%s\"...done" regexp))
    

(defun zephyr-start-receiver ()
  (setq zephyr-lazy-beep-last nil)

  (if (not zephyr-remote-passwd)
      (setq zephyr-remote-passwd
	    (zephyr-remote-read-passwd
	     (format "Password for %s@%s:" 
		     zephyr-remote-userid zephyr-remote-host))))

  (message "Starting remote zephyr process...")
  (require 'telnet-conn)

  (save-excursion
    (set-buffer zephyr-log-buffer)

    (let ((zephyr-remote-start-point (point))
	  (coding-system-for-read 'binary))
      (setq zephyr-process
	    (open-network-stream "zephyr-receive-remote" zephyr-log-buffer
				 zephyr-remote-host 23))

      (process-kill-without-query zephyr-process)

      (telnet-do-negotiation zephyr-process zephyr-log-buffer 
			     zephyr-remote-start-point)
      (goto-char zephyr-remote-start-point))
  
    (zephyr-remote-wait-for-prompt zephyr-remote-login-regexp)
    (process-send-string zephyr-process 
			 (concat zephyr-remote-userid 
				 " -f -c "
				 zephyr-remote-tzc-wrapper-program
				 " \n"))
    
    (zephyr-remote-wait-for-prompt zephyr-remote-passwd-regexp)
    (process-send-string zephyr-process (concat zephyr-remote-passwd "\n"))
    (if (not zephyr-remote-retain-passwd)
	(setq zephyr-remote-passwd nil))
	  
    (zephyr-remote-wait-for-prompt zephyr-remote-reconnect-or-ready-regexp)
    (beginning-of-line)
    (if (looking-at zephyr-remote-reconnect-regexp)
	(progn
	  (goto-char (point-max))
	  (process-send-string zephyr-process "no\n")))
    
    (zephyr-remote-wait-for-prompt zephyr-remote-ready-regexp)
    (beginning-of-line))

  (message "Remote zephyr process has been started.")
  
  (if (fboundp 'start-itimer)
      (progn
	(and (get-itimer "zephyr-remote-keepalive") 
	     (delete-itimer "zephyr-remote-keepalive"))
	(if zephyr-remote-keepalive-interval
	    (start-itimer "zephyr-remote-keepalive"
			  (lambda () 
			    (zephyr-send-to-client '((tzcfodder . ayt))))
			  zephyr-remote-keepalive-interval
			  zephyr-remote-keepalive-interval))))

  (set-process-sentinel zephyr-process 'zephyr-receive-sentinel)
  (set-process-filter zephyr-process 'zephyr-receive-filter))

(defun zephyr-remote-try-shutdown ()
  "try to shut down the remotely running zephyr receive program"
  (interactive)
  (if (and (boundp 'zephyr-process)
	   (processp zephyr-process)
	   (eq 'open (process-status zephyr-process)))
      (let ((pop-up-windows nil))
	(save-excursion
	  (set-buffer zephyr-log-buffer)
	  (process-send-string zephyr-process "\003")
	  (sit-for zephyr-remote-death-time)
	  (goto-char (point-max))))))


(defun zephyr-remote-kill-emacs-18-hook-fn ()
  (zephyr-remote-try-shutdown)
  (run-hooks 'orig-kill-emacs-hook))

;;;(if (or (string-match "XEmacs\\|Lucid" emacs-version)
;;;	(and (boundp 'emacs-major-version)
;;;	     (>= emacs-major-version 19)))
;;;    (add-hook 'kill-emacs-hook 'zephyr-remote-try-shutdown)
;;;  (progn
;;;    (setq orig-kill-emacs-hook kill-emacs-hook)
;;;    (setq kill-emacs-hook 'zephyr-remote-kill-emacs-18-hook-fn)))

(setq non-remote-zephyr-restart-receiver 
  (symbol-function 'zephyr-restart-receiver))

(defun zephyr-restart-receiver ()
  "kill and start another receiver process.  this is a good thing to do if
your kerberos tickets expire, causing all messages authentication to
appear failed."
  (interactive)
  (zephyr-remote-try-shutdown)
  (call-interactively non-remote-zephyr-restart-receiver))
	


;; The following routine is hacked up from one in ange-ftp.el

(defun zephyr-remote-read-passwd (prompt &optional default)
  "Read a password from the user. Echos a . for each character typed.
End with RET, LFD, or ESC. DEL or C-h rubs out.  ^U kills line.
Optional DEFAULT is password to start with."
  (let ((pass (if default default ""))
	(c 0)
	(echo-keystrokes 0)
	(cursor-in-echo-area t))
    (while (and (/= c ?\r) (/= c ?\n) (/= c ?\e))
      (message "%s%s"
	       prompt
	       (make-string (length pass) ?.))
      (setq c (read-char))
      (if (= c ?\C-u)
	  (setq pass "")
	(if (and (/= c ?\b) (/= c ?\177))
	    (setq pass (concat pass (char-to-string c)))
	  (if (> (length pass) 0)
	      (setq pass (substring pass 0 -1))))))
    (substring pass 0 -1)))

(provide 'zephyr-remote)

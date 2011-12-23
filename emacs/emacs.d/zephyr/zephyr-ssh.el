;;
;; This package allows you to run zephyr-mode using a tzc process running
;; on a remote host. You might wish to do this if your local workstation is
;; an architecture for which a local tzc client is not available, or if you're
;; reading zephyr from a machine outside SCS. zephyr-ssh is better than 
;; zephyr-remote because it doesn't propagate your unencrypted password over
;; the network. 
;;
;; - Arup Mukherjee 08/99
;;
;; Note: please read the docs for zephyr-ssh-host! 

(require 'zephyr)

;; The warning in the docs here doesn't apply if "-o StrictHostKeyChecking no"
;; is specified in zephyr-ssh-command-and-args, as it is by default. It has 
;; been retained as a reminder to those who choose to delete that line. 

(defvar zephyr-ssh-host "zimbs.srv.cs.cmu.edu"
  "Host on which to make the remote connection. Note: you MUST have
  successfully ssh'ed into this host at least once using the hostname
  exactly as specified here. If you haven't, ssh may prompt to ask
  you whether you trust this host and want to continue connecting, and
  that will confuse zephyr-ssh, which doesn't look for that prompt. 
  Basically, it will wait for the password: prompt forever.  

  If ssh gets stuck waiting for any prompts, look in the
  zephyr-log-buffer (usually called *log-zephyr*) to see what
  happened.")

(defvar zephyr-ssh-userid (user-login-name)
  "Userid to use when logging in to the remote host")

(defvar zephyr-ssh-tzc "/usr/local/bin/tzc"
  "Path to remote-host tzc")

(defvar zephyr-ssh-local-ssh "/usr/local/bin/ssh"
  "Path to local-host ssh client")

(defvar zephyr-ssh-passwd nil
  "Password to use when logging in to the remote host, or nil 
   to cause the program to prompt you for it")

(defvar zephyr-ssh-retain-passwd nil
  "Whether or not to remember the password on the remote host once it has 
   been specified.")

(defvar zephyr-ssh-passwd-regexp "password:"
  "Regexp matching the password prompt")

;;; (defvar zephyr-ssh-command-and-args
;;;  (list "/usr/local/bin/ssh" zephyr-ssh-host "-l" zephyr-ssh-userid
;;;	"-x" ;; Disables X11 forwarding
;;;	"-o" "PasswordAuthentication yes" 
;;;	"-o" "RSAauthentication no" 
;;;	"-o" "KerberosAuthentication no" 
;;;	"-o" "StrictHostKeyChecking no"  
;;;	"-o" "FallBackToRsh no"
;;;	"exec" zephyr-ssh-tzc "-e" zephyr-exposure))

(defvar zephyr-ssh-ready-regexp "; tzc"
  "Regexp that will be matched indicating that the zephyr-receive-program
   has started and is ready for input")

(defvar zephyr-ssh-death-time 3
  "How long, in seconds, to wait to make sure the remote zephyr receiver 
   process has had a chance to die.")

(defun zephyr-ssh-wait-for-prompt (regexp)
  (message"waiting for remote prompt \"%s\"" regexp)
  (let ((orig-pt (point)))
    (while (not (re-search-forward regexp nil t))
      (accept-process-output)
      (goto-char orig-pt)))
  (message "waiting for remote prompt \"%s\"...done" regexp))
    

(defun zephyr-start-receiver ()
  (setq zephyr-lazy-beep-last nil)

  (let ((password (or zephyr-ssh-passwd
		      (zephyr-ssh-read-passwd
		       (format "Password for %s@%s: "
			       zephyr-ssh-userid zephyr-ssh-host)))))

    (message "Starting remote zephyr process...")

    (save-excursion
      (set-buffer zephyr-log-buffer)

      (setq zephyr-process
	    (apply 'start-process
		   "zephyr-receive-ssh" zephyr-log-buffer
		   ;; zephyr-ssh-command-and-args
		   (list zephyr-ssh-local-ssh zephyr-ssh-host
			 "-l" zephyr-ssh-userid
			 "-x" ;; Disables X11 forwarding
			 "-o" "PasswordAuthentication yes" 
			 "-o" "RSAauthentication no" 
;;;			 "-o" "KerberosAuthentication no" 
			 "-o" "StrictHostKeyChecking no"  
			 "-o" "FallBackToRsh no"
			 "exec" zephyr-ssh-tzc "-e" zephyr-exposure)))
      
      (process-kill-without-query zephyr-process)

      (zephyr-ssh-wait-for-prompt zephyr-ssh-passwd-regexp)
      (process-send-string zephyr-process (concat password "\n"))
      (if zephyr-ssh-retain-passwd
	  (setq zephyr-ssh-passwd password)
	(fillarray password 0))
	  
      (zephyr-ssh-wait-for-prompt zephyr-ssh-ready-regexp)
      (beginning-of-line)))

  (message "Remote zephyr process has been started.")
  
  (set-process-sentinel zephyr-process 'zephyr-receive-sentinel)
  (set-process-filter zephyr-process 'zephyr-receive-filter))

(defun zephyr-ssh-try-shutdown ()
  "try to shut down the remotely running zephyr receive program"
  (interactive)
  (if (and (boundp 'zephyr-process)
	   (processp zephyr-process)
	   (eq 'open (process-status zephyr-process)))
      (let ((pop-up-windows nil))
	(save-excursion
	  (set-buffer zephyr-log-buffer)
	  (process-send-string zephyr-process "\003")
	  (sit-for zephyr-ssh-death-time)
	  (goto-char (point-max))))))

(defun zephyr-ssh-kill-emacs-18-hook-fn ()
  (zephyr-ssh-try-shutdown)
  (if orig-kill-emacs-hook
      (orig-kill-emacs-hook)))

;;; (if (string-match "Lucid" (emacs-version))
;;;    (add-hook 'kill-emacs-hook 'zephyr-ssh-try-shutdown)
;;;  (progn
;;;    (setq orig-kill-emacs-hook kill-emacs-hook)
;;;    (setq kill-emacs-hook 'zephyr-ssh-kill-emacs-18-hook-fn)))

(setq non-remote-zephyr-restart-receiver 
  (symbol-function 'zephyr-restart-receiver))

(defun zephyr-restart-receiver ()
  "kill and start another receiver process.  You might need/want to do this
if your kerberos tickets expire."
  (interactive)
  (zephyr-ssh-try-shutdown)
  (call-interactively non-remote-zephyr-restart-receiver))

;; The following routine is hacked up from one in ange-ftp.el

(defun zephyr-ssh-read-passwd (prompt &optional default)
  "Read a password from the user. Uses read-password if available. If not, 
echos a . for each character typed. End with RET, LFD, or ESC. 
DEL or C-h rubs out.  ^U kills line. Optional DEFAULT is password to start 
with."
  (if (fboundp 'read-passwd)
      (read-passwd prompt nil default)
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
      (substring pass 0 -1))))

(provide 'zephyr-ssh)

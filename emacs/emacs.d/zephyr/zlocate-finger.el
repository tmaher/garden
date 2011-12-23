;;; zlocate-finger.el -- a companion to zephyr.el for locating users
;;; Copyright (C) 1992, 1993 Darrell Kindred (dkindred+@cs.cmu.edu)
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; suggested binding:
;;; (define-key zephyr-mode-map "\ez" 'zephyr-zlocate-finger)

(provide 'zlocate-finger)

(defvar zlocate-program "/usr/local/bin/zlocate"
;;;  (if (file-exists-p "/usr/misc/.zephyr/bin/zlocate")
;;;    "/usr/misc/.zephyr/bin/zlocate"
;;;    "/usr/local/bin/zlocate")
  "*Program used to locate a zephyr subscriber.")

(defvar zlocate-cache-users
  '(("^larry$" . nil)    ; always zlocate larry -- he moves around.
    ("^curly$" . "shemp.cs.cmu.edu") ; we know curly's machine
    (""        . t)  ; cache everyone else
    )
  "*alist for zlocation cache behavior
nil means zlocate always, t means cache, string means use that hostname"
  )

(defvar zlocate-cache nil 
  "use zlocate-flush-cache to clear this.")

(defvar finger-userid nil 
  "internal--don't mess with this")
(defvar finger-output-prefix nil
  "internal--don't mess with this")

(defun zlocate-flush-cache ()
  "Flush the cache which zlocate-finger uses to map users to machines."
  (interactive)
  (setq zlocate-cache nil))

(defun zlocate-insert-hook (hook-var function otherfunc &optional afterflag)
  "Add FUNCTION to the hooklist, HOOK-VAR, before first occurrence of
OTHERFUNC.  If OTHERFUNC doesn't occur, insert at the beginning.
Optional flag AFTERFLAG, if non-nil, means insert the function after
the first occurrence of OTHERFUNC.  If FUNCTION already appears in the
hooklist, do nothing."  
  (if (not (boundp hook-var)) (set hook-var nil))
  (let ((old         (symbol-value hook-var)))
    (if (or (not (listp old)) (eq (car old) 'lambda))
	(setq old (list old)))
    (if (not (memq function old))
	(let* ((rest       (memq otherfunc old))
	       (prev-hooks (reverse (nthcdr (length rest) (reverse old)))))
	  (set hook-var 
	       (if rest
		   (if afterflag
		       (append prev-hooks (list otherfunc function) (cdr rest))
		     (append prev-hooks (list function) rest))
		 (append prev-hooks (list function))))))))

(defun zlocate-cache-zgram-data (msg)
  (let ((sender (cdr (assq 'sender msg)))
	(fromhost (cdr (assq 'fromhost msg))))
    (and sender
	 fromhost
	 (zlocate-cache-pair sender (downcase fromhost)))
  msg))

;;; We want to insert zephyr-cache-zgram-data somewhere reasonable in the 
;;; zephyr-hook-list.  Just before zephyr-insert is fine.  If
;;; zephyr-insert isn't in the list, print a warning message and 
;;; let the user insert it on their own.
(if (boundp 'zephyr-hook-list)
  (if (memq 'zephyr-insert zephyr-hook-list)
      (zlocate-insert-hook 'zephyr-hook-list
			   'zlocate-cache-zgram-data 'zephyr-insert)
    (message "Warning: zephyr-insert not in zephyr-hook-list; location tracking not enabled")
    (sleep-for 1)))

;;; (defvar finger-idle-program "/usr/cs/bin/finger"
;;;  "*For the finger-idle function.")

(defvar finger-host-suffix ".cs.cmu.edu"
  "*Suffix stripped from hostnames when displaying the 
hostname being fingered.")

;;;###autoload
(defun zephyr-zlocate-finger ()
  "zlocate the users who would be the recipients of the zephyr message
at point, then get each user's idle time at the first host listed by
zlocate"
  (interactive)
  (save-excursion
    (let* ((pat (concat "^.+\\("
			zephyr-send-divider-regexp
			"\\)"))
	   (end-recipient (progn (re-search-backward pat)
				 (match-beginning 1)))
	   (recipient (buffer-substring (match-beginning 0) end-recipient))
	   (recip-list (zephyr-send-make-recipient-list recipient)))
      (zlocate-finger-list (mapcar '(lambda (x) (nth 2 x)) recip-list))
      (zephyr-touch-name recipient))))

;;; sleep 1 second between users
;;; ignore empty userids
(defun zlocate-finger-list (userids &optional quiet)
  (cond 
   ((null userids)   nil)
   ((= 0 (length (car userids)))  
    (zlocate-finger-list (cdr userids) quiet))
   (t (zlocate-finger (car userids) quiet (cdr userids))
      (cond
       ((> (length userids) 1)
	(sleep-for 1)
	(zlocate-finger-list (cdr userids) t))))))
	
;;;###autoload
(defun zlocate-finger (userid &optional quiet wait)
  "zlocate USERID and then finger at the first machine listed,
stripping off the suffix zlocate-common-suffix if present.
Optional arg QUIET, if non-nil, means don't touch message buffer
until everything is done.  Optional arg WAIT, if non-nil, means
don't return until the finger is done."
  (interactive "suserid: ")
  (if (null quiet)
      (message (concat userid ":")))
  (let ((hostname   (zlocate-with-cache userid)))
    (if hostname
	;; strip off realm, if any
	(let ((userid-no-realm  (if (string-match "^\\([^@]*\\)@" userid)
				    (substring userid (match-beginning 1)
					       (match-end 1))
				  userid)))
	  (finger-idle userid-no-realm hostname quiet wait)))))

;;; assumes there is only one entry per user
(defun zlocate-cache-flush-user (userid cache)
  (let ((c cache)
	(result nil))
    (while (not (null c))
      (if (not (string-equal (car (car c)) userid))
	  (setq result (cons (car c) result)))
      (setq c (cdr c)))
    result))

(defun zlocate-cache-pair (userid hostname)
  (setq zlocate-cache (cons (cons userid hostname) 
			    (zlocate-cache-flush-user userid zlocate-cache))))


(defun zlocate-lookup-userid (cache-users userid)
  (if (null cache-users)
      (error "bad zlocate-cache-users list")
    (if (string-match (car (car cache-users)) userid)
	(cdr (car cache-users))
      (zlocate-lookup-userid (cdr cache-users) userid))))

;;; zlocate if necessary, or used cached entry (see zlocate-cache-users)
(defun zlocate-with-cache (userid)
  (let ((cache-behavior    (zlocate-lookup-userid zlocate-cache-users userid))
	(cached-hostname   (assoc userid zlocate-cache)))
    (if (stringp cache-behavior)
	cache-behavior
      (if (and cache-behavior
	       cached-hostname)
	  (cdr cached-hostname)
	(let ((hname  (zlocate userid)))
	  (if hname
	      (zlocate-cache-pair userid hname))
	  hname)))))


;;; Return the first hostname listed for USERID by zlocate-program,
;;; or nil and print message on error (or hidden/not-logged-in)
(defun zlocate (userid)
  (save-excursion
    (let ((tmpbuf (get-buffer-create "*zlocate-temp*")))
      (set-buffer tmpbuf)
      (erase-buffer)
      (call-process zlocate-program nil t nil userid)
      (goto-char (point-min))
      (if (not (looking-at "[A-Za-z0-9-]+\\.[A-Za-z0-9-]+"))  ; a hostname
	  (progn 
	    (end-of-line)
	    (message (concat userid ": " 
			     (buffer-substring (point-min) (point))))
	    (kill-buffer tmpbuf)
	    nil)
	(progn
	  (re-search-forward "\\([^ ]*\\)")
	  (let* ((caps    (buffer-substring (match-beginning 1) (match-end 1)))
		 (hostname (downcase caps)))
	    (kill-buffer tmpbuf)
	    hostname))))))

;;;###autoload
(defun finger-idle (userid host &optional quiet wait)
"get idle times for USERID at HOST.  Optional third arg QUIET,
if non-nil, means don't touch message buffer until finger is done.
Optional fourth arg WAIT, if non-nil, means don't return until
the finger is done."
  (interactive "suserid: \nshost: ")
  (save-excursion
    (let* ((suffix-loc     (if finger-host-suffix
			       (string-match finger-host-suffix host)
			     nil))
	   (abbrev-host    (substring host 0 suffix-loc))
	   (output-prefix  (concat userid "@" abbrev-host ": ")))
      (if (null quiet)
	  (message output-prefix))
      (let ((tmpbuf     (generate-new-buffer "*finger-temp*")))
	(set-buffer tmpbuf)
	(erase-buffer)
	(make-local-variable 'finger-userid)
	(make-local-variable 'finger-output-prefix)
	(setq finger-userid userid)
	(setq finger-output-prefix output-prefix)
	(let ((finger-proc   (open-network-stream "finger" tmpbuf host 79)))
	  (set-process-sentinel finger-proc 'finger-sentinel)
	  (process-send-string finger-proc "\n")
	  ;; under lemacs (before 19.12), we have to sleep while 
          ;; waiting for the 
	  ;; process to finish, because otherwise it won't
	  ;; run the finger-sentinel until some other event happens. Ugh.
	  ;; (we also want to wait if we're doing several fingers)
	  (if (or wait
		  (and (string-match "XEmacs\\|Lucid" emacs-version)
		       (boundp 'emacs-major-version)
		       (<= emacs-major-version 19) (< emacs-minor-version 12)))
	;      (progn
		(while (eq 'open (process-status finger-proc))
		  ;; if we put in an actual 0.1 here then emacs v18 chokes
		  ;; when reading the .elc file.
		  ;; (sleep-for (string-to-number "0.1"))  -- no such function
		  ;; (sleep-for (read "0.1"))
		  (accept-process-output nil 1)
		  )
		;; (finger-sentinel finger-proc))
	    ))))))

(defun finger-sentinel (proc &optional sig)
  (save-excursion
    (let ((status (process-status proc))
	  (buf    (process-buffer proc)))
      (if (eq status 'closed)
	  (progn (set-buffer buf)
		 (goto-char (point-min))
		 (let ((result (finger-parse-output finger-userid)))
		   (if (> (length result) 0)
		       (message (concat finger-output-prefix result))
		     (message (concat finger-output-prefix "not logged in"))))
		 (cond ((buffer-name buf)
			(kill-buffer buf)
			(delete-process proc))))))))

(defun finger-parse-output (userid)
  (goto-char (point-min))
  (let ((case-fold-search t)
	(idle-cols        '(35 . 39))	;default values..
	(output           ""))
    (while (and (not (eobp))
		(not (looking-at "^[ \t]*user"))
		(not (looking-at "^[ \t]*login"))
		(= 0 (forward-line 1))))
    (if (re-search-forward "idle\\(time\\)?" nil t)
	(let ((bol  (progn (beginning-of-line) (point))))
	  (setq idle-cols
		(cons (- (- (match-beginning 0) 1) bol)
		      ; (- (match-beginning 0) bol)
		      (1+ (- (match-end 0) bol))))
	  (forward-line 1))
      (goto-char (point-min)))
    (while (< (point) (point-max))
      (if (looking-at (concat (regexp-quote userid) " "))
	  (let* ((bol      (point))
		 (idletime (buffer-substring (+ bol (car idle-cols))
					     (+ bol (cdr idle-cols))))
		 (eol      (save-excursion (forward-line 1) (point)))
		 ;; (logintime "")
		 (location  ""))
	    (goto-char (+ bol (cdr idle-cols)))
	    (if (re-search-forward ":[0-9][0-9] " eol t)
		(progn ;; (setq logintime
			;;     (buffer-substring (+ bol (cdr idle-cols) 1)
			;;		       (1- (match-end 0))))
		       (setq location (buffer-substring (match-end 0) eol))))
	    (setq location
		  (if (string-match "\\([^][)( \r\n]+\\)[][)(\r\n ]*$" location)
		      (substring location (match-beginning 1) (match-end 1))
		    "--"))
	    (if (string-match "^\\([a-z][^.]+\\)\\." location)
		(setq location (substring location 
					    (match-beginning 1)
					    (match-end 1))))
	    (string-match "\\([^ ]*\\) ?$" idletime)
	    (let* ((idle-1 (substring idletime
				      (match-beginning 1)
				      (match-end 1)))
		   (idle-2 (if (or (string-equal idle-1 "")
				   (string-equal idle-1 "-"))
			       "busy"
			     idle-1)))
	      (setq output (concat output "(" idle-2 ", " location ") ")))))
      (forward-line 1))
    output))

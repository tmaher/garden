;; Functions to enable zephyr-logging
;; 
;; Arup Mukherjee (arup+@cmu.edu) and Darrell Kindred (dkindred+@cmu.edu) 6/94

;; To enable logging, put something similar to the following into your
;; .emacs AFTER you have created your zephyr buffer(s), and AFTER you
;; have set zephyr-send-function (if you do so). Modify the names
;; of the log file and zephyr buffer as appropriate, of course.
;;
;;   (require 'zephyr-logging)
;;   (zephyr-enable-logging "~/.zephyrlog" "*zephyr-personal-zgrams*")
;;
;; To log only sent zephyrgrams or only received zephyrgrams, use
;; zephyr-enable-send-logging or zephyr-enable-receive-logging.  
;; For example,
;;   (zephyr-enable-send-logging "~/.zephyrlog" "*zephyr-nonpersonal-zgrams*")
;;
;; To turn off all logging in a buffer, use zephyr-disable-logging.
;;

(defvar zephyr-log-message-p-function 'zephyr-log-always
  "This function will be called with two arguments: a zgram, and incoming-p
(t or nil).  If the function returns non-nil, the message will be logged.")

(defvar zephyr-log-receive-file-name nil
  "Don't modify this--use the FILENAME argument to zephyr-enable-logging
or zephyr-enable-receive-logging.")

(defvar zephyr-log-send-file-name nil
  "Don't modify this--use the FILENAME argument to zephyr-enable-logging
or zephyr-enable-send-logging.")

(defvar zephyr-nonlogging-send-function nil
  "Don't modify this.")

(defun zephyr-log-always (msg incoming-p)
  "For use as zephyr-log-message-p-function.  Logs everything."
  t)

(defun zephyr-log-outgoing-and-personal-incoming (msg incoming-p)
  "For use as zephyr-log-message-p-function.  Logs all outgoing messages,
and all incoming non-broadcast messages on the PERSONAL instance."
  (or (not incoming-p) 
      (and (zephyr-personal-msg-p msg)
	   (string-equal (downcase (cdr (assq 'instance msg))) "personal"))))

(defun zephyr-enable-logging (filename &optional buffer)
  "Log sent *and* received zephyrgrams to FILENAME. Only zephyrgrams that show
up in the existing buffer BUFFER are logged. Defaults to current buffer.
If FILENAME is nil, disable logging."

  (interactive "FLog sent and received zephyrs to file: ")

  (zephyr-enable-send-logging filename buffer)
  (zephyr-enable-receive-logging filename buffer))


(defun zephyr-enable-receive-logging (filename &optional buffer)
  "Log received zephyrgrams to FILENAME. Only zephyrgrams that show
up in the existing buffer BUFFER are logged. Defaults to current buffer.
If FILENAME is nil, disable receive logging."

  (interactive "FLog received zephyrs to file: ")

  (let* ((buffer (if (null buffer) (current-buffer) buffer))
	 (zbuffer (get-buffer buffer))
	 (fname  (cond 
		  ((null filename) nil)
		  ((stringp filename) (expand-file-name filename))
		  (t (error "log file name was neither a string nor nil!")))))
    (if (null zbuffer)
	(error "zephyr buffer \"%s\" was not found" buffer))
    
    (save-excursion
      (set-buffer zbuffer)

      (if (not (eq 'zephyr-mode major-mode))
	  (error "buffer \"%s\" is not in zephyr-mode!" (buffer-name)))

      (if (memq 'zephyr-log-incoming zephyr-insert-hook-list)
	  (setq zephyr-log-receive-file-name fname)
	(progn
	  (make-local-variable 'zephyr-insert-hook-list)
	  (make-local-variable 'zephyr-log-receive-file-name)
	  (setq zephyr-log-receive-file-name fname)
	  (setq zephyr-insert-hook-list 
		(cons 'zephyr-log-incoming 
		      zephyr-insert-hook-list)))))))


(defun zephyr-enable-send-logging (filename &optional buffer)
  "Log sent zephyrgrams to FILENAME. Only zephyrgrams that show up in
the existing buffer BUFFER are logged. Defaults to current buffer.
If FILENAME is nil, disable send logging."

  (interactive "FLog sent zephyrs to file: ")

  (let* ((buffer (if (null buffer) (current-buffer) buffer))
	 (zbuffer (get-buffer buffer))
	 (fname  (cond 
		  ((null filename) nil)
		  ((stringp filename) (expand-file-name filename))
		  (t (error "log file name was neither a string nor nil!")))))
    (if (null zbuffer)
	(error "zephyr buffer \"%s\" was not found" buffer))
    
    (save-excursion
      (set-buffer zbuffer)

      (if (not (eq 'zephyr-mode major-mode))
	  (error "buffer \"%s\" is not in zephyr-mode!" (buffer-name)))

      (if (eq zephyr-send-function 'zephyr-send-and-log)
	  (setq zephyr-log-send-file-name fname)
	(progn
	  (make-local-variable 'zephyr-log-send-file-name)
	  (make-local-variable 'zephyr-send-function)
	  (make-local-variable 'zephyr-nonlogging-send-function)
	  (setq zephyr-log-send-file-name fname)
	  (setq zephyr-nonlogging-send-function zephyr-send-function)
	  (setq zephyr-send-function 
		'zephyr-send-and-log))))))


(defun zephyr-disable-logging (&optional buffer)
  "Stop logging zephyrgrams in BUFFER.  Defaults to current buffer."
  (interactive)
  (zephyr-enable-logging nil buffer))


(defun zephyr-log-incoming (msg)
  (if (and (boundp 'zephyr-log-receive-file-name) 
	   zephyr-log-receive-file-name
	   (funcall zephyr-log-message-p-function msg t))
      (let* ((msg-banner (cdr (assq 'banner msg)))
	     (msg-text   (cdr (assq 'print-as msg)))
	     (msg-length (length msg-text))
	     (outfile    zephyr-log-receive-file-name))
	(save-excursion
	  (set-buffer (get-buffer-create " *zlog*"))
	  (delete-region (point-min) (point-max))
	  (insert msg-banner msg-text)
	  (if (or (= 0 msg-length)
		  (not (eq ?\n (aref msg-text (- msg-length 1)))))
	      (insert "\n"))
	  (write-region (point-min) (point-max) outfile t 'no-msg))))
  msg)


(defun zephyr-send-and-log (msg to-list)
  (if (and (boundp 'zephyr-log-send-file-name) 
	   zephyr-log-send-file-name
	   (funcall zephyr-log-message-p-function msg nil))
      (let ((outfile zephyr-log-send-file-name))
	(save-excursion
	  (set-buffer (get-buffer-create " *zlog*"))
	  (delete-region (point-min) (point-max))
	  (insert (mapconcat 'zephyr-make-text-recipient-class to-list ",")
		  zephyr-send-divider
		  msg "\n")
	  (write-region (point-min) (point-max) outfile t 'no-msg))))
  (funcall zephyr-nonlogging-send-function msg to-list))


(provide 'zephyr-logging)

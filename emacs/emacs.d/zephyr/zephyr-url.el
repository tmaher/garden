;;; zephyr-url.el    makes URLs and filenames hot-links in zephyrgrams
;;;
;;; Copyright (c)1993 Darrell Kindred <dkindred@cs.cmu.edu>
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


;;; This file provides hooks to use hot-url in zephyr-mode, 
;;; making hot-links out of URLs and/or absolute filenames.
;;;
;;; To follow one of these links, move point to it and press return,
;;; or, in XEmacs (lemacs), just click the middle mouse button on the link.
;;;
;;; To use this, just put
;;;      (require 'zephyr-url)
;;; at the end of your zephyr customizations, and set
;;; hot-url-fetch-url-function appropriately (see hot-url.el).
;;;
;;; The following packages are used to provide this feature:
;;;     hot-link.el             support for creating & following hot links
;;;     hot-url.el              turns urls and filenames into hot links
;;;     w3-fetch-url.el         function for using w3 to fetch a URL
;;;     mosaic-fetch-url.el     function for using NCSA Mosaic to fetch a URL
;;;     netscape-fetch-url.el   function for using Netscape to fetch a URL
;;;     raw-fetch-url.el        function for using lynx -source to fetch a URL

(require 'zephyr)
(require 'hot-url)

;; These variables are obsolete.  Use hot-url-default-hot-types instead.
;;
;; (defvar zephyr-hot-urls t
;;  "If non-nil, make URLs in received zephyrgrams into hot-links.")
;; (defvar zephyr-hot-msgids t
;;  "If non-nil, make message-ids (in angle brackets) in received zephyrgrams
;; into hot-links.")
;; (defvar zephyr-hot-filenames t
;;  "If non-nil, make absolute filenames in received zephyrgrams into hot-links.")

;; XXX could this be done as another type in the hot-url type registry?
(defvar zephyr-hot-senders nil
  "*If non-nil, make the sender's userid a hot-link to his/her www home page.")

;;;  default key bindings -- hit return to follow the link the cursor is on.
;;;                         in XEmacs (lemacs), press button2 to follow a link.

(defvar zephyr-hot-url-map nil
  "Keymap used for url extents in zephyr buffers")

(defun zephyr-url-init-bindings ()
  (if (fboundp 'extent-property) ;; lemacs 19.9 & later
      (progn
	(setq zephyr-hot-url-map (make-sparse-keymap))
	(define-key zephyr-hot-url-map 'button2 'hot-link-follow-mouse)
	(define-key zephyr-hot-url-map "\C-m" 'hot-link-follow-point))
    (setq zephyr-hot-url-map nil)
    (if (string-match "XEmacs\\|Lucid" emacs-version)
	(hot-link-bind-maybe-follow-mouse zephyr-mode-map
					  'button2
					  (global-key-binding 'button2)))
    (hot-link-bind-maybe-follow-point zephyr-mode-map
				      "\C-m"
				      (global-key-binding "\C-m"))))

(defun zephyr-url-init-menu-items ()
  ;; tack on another couple of menu items to the zephyr menu
  (if (and (string-match "XEmacs\\|Lucid" emacs-version)
	   zephyr-menu)
      (setq zephyr-menu 
	    ;; XXX should we (make-local-variable 'hot-url-default-hot-types) below?
	    (append zephyr-menu
		    '(("Hot URL Options"
		       ["Hot URLs"
			(if (memq 'url hot-url-default-hot-types)
			    (setq hot-url-default-hot-types
				  (delq 'url hot-url-default-hot-types))
			  (setq hot-url-default-hot-types (cons 'url hot-url-default-hot-types)))
			:style toggle :selected (memq 'url hot-url-default-hot-types)]
		       ["Hot filenames"
			(if (memq 'filename hot-url-default-hot-types)
			    (setq hot-url-default-hot-types
				  (delq 'filename hot-url-default-hot-types))
			  (setq hot-url-default-hot-types
				(cons 'filename hot-url-default-hot-types)))
			:style toggle :selected (memq 'filename hot-url-default-hot-types)]
		       ["Hot message-ids"
			(if (memq 'msgid hot-url-default-hot-types)
			    (setq hot-url-default-hot-types
				  (delq 'msgid hot-url-default-hot-types))
			  (setq hot-url-default-hot-types
				(cons 'msgid hot-url-default-hot-types)))
			:style toggle :selected (memq 'msgid hot-url-default-hot-types)]
		       ["Hot senders"
			(setq zephyr-hot-senders (not zephyr-hot-senders))
			:style toggle :selected zephyr-hot-senders]
		       "---"
		       ["Use w3 to fetch URLs" 
			hot-url-use-w3
			:style radio :selected 
			(eq hot-url-fetch-url-function
			    'fetch-url-using-w3)]
		       ["Use Mosaic to fetch URLs" 
			hot-url-use-mosaic
			:style radio :selected 
			(eq hot-url-fetch-url-function
			    'mosaic-fetch-url)]
		       ["Use Netscape to fetch URLs" 
			hot-url-use-netscape
			:style radio :selected 
			(eq hot-url-fetch-url-function
			    'netscape-fetch-url)]
		       ["Use raw-fetch to fetch URLs" 
			hot-url-use-raw-fetch
			:style radio :selected 
			(eq hot-url-fetch-url-function
			    'raw-fetch-url)])
		      "---"))))
  )

(defvar zephyr-url-init-done nil)

(defun zephyr-url-init ()
  (if (not zephyr-url-init-done)
      (progn
	(zephyr-url-init-bindings)

	(if (not (memq 'zephyr-make-hot-urls zephyr-insert-hook-list))
	    (setq zephyr-insert-hook-list (append zephyr-insert-hook-list
						  '(zephyr-make-hot-urls))))

	(zephyr-url-init-menu-items)
	(setq zephyr-url-init-done t)
	)))

(defun zephyr-homepage-url (sender)
  "Return a url for the home page for this zephyr sender."
  (let ((userid sender)
	(realm  (if (boundp 'zephyr-realm) zephyr-realm nil)))
    (if (string-match "\\(.*\\)@\\([^@]*\\)" sender)
	(progn
	  (setq userid (substring sender (match-beginning 1) (match-end 1)))
	  (setq realm  (substring sender (match-beginning 2) (match-end 2)))))
    (if (string-match "\\([^.]*\\)\\." userid)
	(setq userid (substring userid (match-beginning 1) (match-end 1))))
    (cond
     ((or (null realm)
	  (string-equal realm "CS.CMU.EDU"))
      (concat "http://gs213.sp.cs.cmu.edu/prog/findhome?userid=" userid))
     (t nil))))      

;;; msg should contain (body-begin . pos) and (body-end . pos) pairs.
;;; since we might change the message, we have to update 
;;; body-end and msg-end
(defun zephyr-make-hot-urls (msg)
  "Make URLs and/or absolute filenames in MSG into hot-links
 (depending on values of zephyr-hot-urls and zephyr-hot-filenames).
Intended for use in zephyr-insert-hook-list."
  (let* ((body-begin   (cdr (assq 'body-begin msg)))
	 (body-end     (cdr (assq 'body-end msg)))
         (msg-begin    (cdr (assq 'msg-begin msg)))
         ;; (msg-end      (cdr (assq 'msg-end msg)))
	 ;; (body-end-marker  (copy-marker body-end))
	 ;; (msg-end-marker   (copy-marker msg-end))
	 )
    (if zephyr-hot-senders
	(let* ((sender-end  (save-excursion
			      (goto-char msg-begin)
			      (skip-chars-forward "a-zA-Z0-9\\-@\\.")
			      (point)))
	       (url         (zephyr-homepage-url 
			     (buffer-substring msg-begin sender-end))))
	  (if url
	      (hot-link-make msg-begin sender-end 'hot-url::fetch-it
			     (list 'url url) nil zephyr-hot-url-map))))
    (hot-url-make-all-hot body-begin body-end zephyr-hot-url-map
			  `((time-secs . ,(cdr (assq 'time-secs msg)))
			    (personal . ,(zephyr-personal-msg-p msg))))
    ;; NOTE: this stuff is no longer necessary as of zephyr.el 2.5.0,
    ;;       since these things are markers.  Actually it was probably
    ;;       never really necessary since I think we never modify the
    ;;       contents of the buffer.
    ;; ;; update body-end and msg-end
    ;; (let ((new-body-end (marker-position body-end-marker))
    ;;      (new-msg-end (marker-position msg-end-marker)))
    ;;  (move-marker body-end-marker nil)
    ;;  (move-marker msg-end-marker nil)
    ;;  ;; don't bother removing the old ones, just cons...
    ;;  (append (list (cons 'body-end new-body-end)
    ;;                (cons 'msg-end new-msg-end))
    ;;          msg))
    msg
))

(defvar zephyr-url-suppress-bumbot-frame nil
  "*If t, zephyr-url-try-snapshot will suppress the bumbot frame")

(defun zephyr-url-try-snapshot (url info)
  "Put this function on the hot-url-convert-url-functions list to convert
urls appearing in zgrams to references to the relevant zarchive snapshot."
  (let ((time-secs (cdr (assq 'time-secs info))))
    (if (or ; (cdr (assq 'personal info))
	    (null time-secs))
	url				; no change
      (concat "http://zarchive.srv.cs.cmu.edu/prog/fetch_url?"
	      (if zephyr-url-suppress-bumbot-frame "noframe=1&" "")
	      "url="
	      (zephyr-url::url-quote url)
	      "&timesecs="
	      (mapconcat 'int-to-string time-secs "."))
      )))

(defun zephyr-url::url-quote (s)
  (save-excursion
    (set-buffer (generate-new-buffer " zephyr-url::url-quote"))
    (insert s)
    (goto-char (point-min))
    (while (re-search-forward "[^a-zA-Z0-9$_.!*'(),-]" nil t)
      (replace-match (format "%%%02x" (char-after (match-beginning 0)))))
    (setq s (buffer-substring (point-min) (point-max)))
    (kill-buffer (current-buffer))
    s))

;;; XXX this is a violation of the new 20.x convention that packages
;;;     don't "do anything" when they're loaded.  What to do...
(zephyr-url-init)

(provide 'zephyr-url)

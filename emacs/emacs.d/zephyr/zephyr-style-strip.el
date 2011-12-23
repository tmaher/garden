;;; zephyr-style-strip.el - a zwgc-style stripper for zephyr.el
;;; Copyright 1998 Brian Kidder (kidder@qualcomm.com)
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

;;; This code is essentially the translated C-code for stripstyle
;;; command from zwgc

;;; It is intended for use with zephyr.el version 2.4.6 or later
;;; It may wpork woith earlier versions, but I haven't tested extensively.

;;; Add any of the following functions to the zephyr-hook-list
;;;  zephyr-style-strip-body
;;;  zephyr-style-strip-signature
;;;  zephyr-style-strip-opcode
;;;  zephyr-style-strip-all

;;; HISTORY

;;; 02 Sep 1999, Tom Maher <tardis@ece.cmu.edu>
;;;    Started keeping history.
;;;    Added zephyr-style-strip-on variable to turn this off easily.

(defvar zephyr-style-strip-on t
  "Turns style-stripping off when set to nil, on when set to t.
Default is t")

(defun zephyr-style-otherside (ch)
  (cond ((= ch 40) 
	 ;;; (
	 41)
	((= ch 91)
	 ;;; [
	 93)
	((= ch 123)
	 ;;; {
	 125)
	((= ch 60)
	 ;;; <
	 62)
	(t ch)))

(defun zephyr-style-alphanumericp (c)
  (cond ((and (>= c ?A) (<= c ?Z)) t)
	((and (>= c ?a) (<= c ?z)) t)
	((and (>= c ?0) (<= c ?9)) t)
	(t nil)))

(defun zephyr-style-env_length (string index)
  (setq len 0)
  (loop while (and (< (+ index len) (length string))
		   (or (zephyr-style-alphanumericp (aref string (+ index len)))
		       (= (aref string (+ index len)) 95)))
	do (setq len (+ len 1)))
  
  (if (and (< (+ index len) (length string))
	   (or (= (aref string (+ index len)) 40);; (
	       (= (aref string (+ index len)) 123);; {
	       (= (aref string (+ index len)) 91);; [
	       (= (aref string (+ index len)) 60)));; <
      len -1))

(defun zephyr-style-strip-string (string)
  "Return STRING stripped of all zephyr styling"
  (interactive)
  (setq temp "")
  (setq possible-tagp t)
  (loop with ch_stack = '()
	with index = 0
	with otherchar = 0
	while (< index (length string))
	do 
	(cond ((and (= (aref string index) 64);;@
		    possible-tagp)
	       (let ((len (zephyr-style-env_length string (+ index 1))))
		 (if (>= len 0)
		     (progn
		       (setq otherchar 0)
		       (if (or (and (eq len 4) (eq (+ index 1) (string-match "font" string index)))
			       (and (eq len 5)
				    (eq (+ index 1) (string-match "color" string index))))
			   (setq otherchar 128))
		       (setq otherchar (+ otherchar (zephyr-style-otherside 
						     (aref string (+ index len 1)))))
		       (setq ch_stack (cons otherchar ch_stack))
		       (setq index (+ index len 2)))
		   (setq possible-tagp nil))))
	    
	      ((and (not (endp ch_stack))
		    (= (aref string index) (mod (first ch_stack) 128)))
	       (setq ch_stack (rest ch_stack))
	       (setq index (+ index 1))
	       (setq possible-tagp t))
	    
	      ((and (not (endp ch_stack))
		    (>= (first ch_stack) 128))
	       (setq index (+ index 1))
	       (setq possible-tagp t))
	    
	      (t
	       (setq temp (concat temp (char-to-string (aref string index))))
	       (setq index (+ index 1))
	       (setq possible-tagp t))))
  temp)

(defun zephyr-style-strip-item (msg item)
  (let ((entry (assq item msg))) 
    (if (and zephyr-style-strip-on entry)
	(setcdr entry (zephyr-style-strip-string (cdr entry)))))
  msg)

(defun zephyr-style-strip-body (msg)
  "Strip out zephyr styles within the body of a zephyr message
Add to your zephyr-hook-list"
  (zephyr-style-strip-item
   (zephyr-style-strip-item msg 'body) 'print-as))

(defun zephyr-style-strip-signature (msg)
  "Strip out zephyr styles within the signature of a zephyr message
Add to your zephyr-hook-list"
  (zephyr-style-strip-item msg 'signature))

(defun zephyr-style-strip-instance (msg)
  "Strip out zephyr styles within the signature of a zephyr message
Add to your zephyr-hook-list"
  (zephyr-style-strip-item msg 'instance))

(defun zephyr-style-strip-opcode (msg)
  "Strip out zephyr styles within the opcode of a zephyr message
Add to your zephyr-hook-list"
  (let ((entry (assq 'opcode msg)))
    (setcdr entry (make-symbol (zephyr-style-strip-string
				(symbol-name (cdr entry)))))) msg)

(defun zephyr-style-strip-all (msg)
  "Strip out zephyr styles within a zephyr message
Add to your zephyr-hook-list"
  (zephyr-style-strip-body (zephyr-style-strip-signature 
			    (zephyr-style-strip-opcode msg))))

(provide 'zephyr-style-strip)

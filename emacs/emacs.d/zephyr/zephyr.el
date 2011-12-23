;;; zephyr.el - an interface to the zephyr message system
;;; Copyright (c) 1992, 1993 Scott Draves (spot@cs.cmu.edu)
;;; Copyright (c) 1995, 1998 Darrell Kindred <dkindred@cs.cmu.edu>
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

;;; This is version 2.5.19
;;; You should use tzc version 2.3 or later.

;;; Darrell Kindred <dkindred@cs.cmu.edu> is the current zephyr.el 
;;; maintainer.

;;; Thanks to Michael Duggan, Mark Eichin, Todd Kaufmann, Nick
;;; Thompson, and Hank Wan for their contributions.

;;; If you write C programs which talk to emacs, you should check out
;;; lread.h and lread.c in the tzc program.  These provide a
;;; convenient way to communicate structured data.

;;; send and receive zephyrgrams with a convenient and flexible
;;; interface.  some people say the interface feels like IRC.
;;; here's a quick feature list:
;;;    asynchronously send and receive messages from a single buffer
;;;    handles instances, multiple destinations
;;;    aliases (can translate one name to many)
;;;    "lazy" beep for notification
;;;    sender/receiver history
;;;    filter messages based on sender, instance, receiver, opcode, etc.
;;;    multiple buffers, each with different filters
;;;    highly customizeable
;;;
;;; there's some support for encrypted instances, but it no longer
;;; works.  
;;;
;;; to use, copy the following lines into your .emacs:
;;;
;;;  (load-library "zephyr")
;;;  (zephyr-new-buffer)
;;;
;;; this mode requires the tzc program which should have come with the
;;; distribution.  Send e-mail to dkindred@cs.cmu.edu to get sources
;;; for tzc.
;;;
;;; Bug reports, improvements, and suggestions are always welcome.
;;; Feel free to send mail to dkindred@cs.cmu.edu.


;;; TODO
;;; - make the exposure be a sexp we send to tzc rather than a
;;;   command line option.  eliminate other options same way.
;;;   [partially done]
;;; - if you send to multiple destinations, the "... done" messages
;;;   cover each other up, so you can't really see all of them.

;;; CHANGES
;;; 22 Aug 2000 - (2.5.19) call set-buffer-process-coding-system after creating
;;;			   the process, not before.  fix zephyr-touch to handle
;;;                        userids with digits.
;;; 22 Aug 2000 - (2.5.18) use zephyr-personal-msg-p in zephyr-notify-with-beep
;;; 21 Aug 2000 - (2.5.17) mule patch from md5i, zephyr-quote-message tweak 
;;; 28 Jul 2000 - (2.5.16) apply fsf emacs 20 / multilingual patches from md5i,
;;;               add "^(plusplus:.*)$" to zephyr-history-ignore-regexp,
;;;		  add zephyr-quote-message (C-c C-y) from md5i
;;; 13 Mar 2000 - (2.5.15) fix zephyr-add-hook bug when before=nil
;;; 08 Nov 1999 - (2.5.14) add protection against anon-* broadcasts
;;; 29 Jun 1999 - (2.5.13) (finally) add zephyr-add-hook
;;; 21 Apr 1999 - (2.5.12) fix zephyr-default-insert-p bug
;;; 18 Apr 1999 - (2.5.11) make zephyr-warn-empty-message default t
;;; 16 Dec 1998 - (2.5.10) replace save-restriction with homebrewed solution
;;; 16 Dec 1998 - (2.5.09) zephyr-insert does save-restriction & widen
;;; 16 Oct 1998 - (2.5.08) support (insert-at . top)
;;; 15 Oct 1998 - (2.5.07) allow (query . q37) in zephyr-filters
;;; 02 Oct 1998 - (2.5.06) fix (zephyr-make-string nil) to return ""
;;; 26 Sep 1998 - (2.5.05) replace copy-tree w/ copy-keymap for fsfmacs
;;; 19 Sep 1998 - (2.5.04) prepare for tzc making class and opcode strings
;;; 26 Aug 1998 - (2.5.03) fsfmacs menu support from md5i@cs.cmu.edu
;;; 25 Aug 1998 - (2.5.02) add zephyr-process-connection-type variable
;;; 11 Jul 1998 - (2.5.01) ignore extra zgram fields (beyond #2), like zwgc
;;; 11 Jun 1998 - (2.5.0) {msg,body}-{begin,end} are now markers
;;; 06 May 1998 - (2.4.8) fix minor bug for send-divider containing newline
;;; 14 Jan 1998 - (2.4.7) add support for ayt-response from tzc,
;;;               make optional no-prefix arg to zephyr-new-buffer
;;; 13 Oct 1997 - (2.4.6) fix zephyr-notify-with-message-p; add various
;;;               (mostly cmucs-specific) entries to z-history-ignore-regexp
;;; 23 Apr 1997 - (2.4.5) zephyr-notify-with-message-p variable
;;; 25 Feb 1997 - (2.4.4) zephyr-buffer-substring-no-properties (for fsfmacs)
;;; 07 Nov 1996 - (2.4.3) "message" fn takes format-style args, not raw string;
;;;               fix zephyr-touch to handle x-realm broadcasts properly
;;; 08 Aug 1996 - (2.4.2) listen-temporarily excludes PINGs (opcode "" only)
;;; 31 Jan 1996 - (2.4.1) limit zbuffer size when messages *arrive*
;;; 20 Oct 1995 - disable undo in *log-zephyr* to save a little memory,
;;;               only define cadr & friends if not already defined.
;;; 16 Aug 1995 - zephyr-limit-buffer-size more efficient (trims to 0.9*max)
;;; 23 Jul 1995 - fix zephyr-touch-name yet again
;;; 23 Jul 1995 - special case in zephyr-parse-fields for urgent zgrams
;;; 07 Jun 1995 - fixed zephyr-send bug when sending to "(cl:inst),bovik"
;;; 01 Mar 1995 - added zephyr-warn-empty-message variable (default nil),
;;;               and zephyr-check-message.  prompts for facilities.announce.
;;;               added zephyr-auto-reply fn.
;;; 15 Feb 1995 - removed zephyr-notify-with-scroll from the default hook-list
;;; 06 Feb 1995 - allow (instance@realm) syntax
;;; 11 Dec 1994 - add "listen to class" menu item
;;; 09 Dec 1994 - if message has only one field, treat body as empty
;;; 09 Dec 1994 - finally nuked all traces of ignore & accept regexps
;;; 09 Dec 1994 - fixed small zephyr-next-destination bug
;;; 08 Dec 1994 - (2.4) support for sending to non-MESSAGE classes!
;;; 01 Nov 1994 - zephyr-notify-with-scroll fix for fsf19
;;; 31 Oct 1994 - add zephyr-strip-text-properties for fsf19
;;; 26 Oct 1994 - remove \n from default signature
;;; 01 Jul 1994 - (2.3) lemacs -> xemacs
;;; 08 Jun 1994 - (2.2-beta11) fix bug in zephyr-time-lessp
;;; 07 May 1994 - (2.2-beta10) add zephyr-before-send-hook, and change
;;;               zephyr-after-send-hook to pass msg & to-list as args.
;;; 07 Apr 1994 - (2.2-beta9) don't create a menubar if none already exists
;;; 13 Mar 1994 - (2.2-beta8) cross-realm support (see zephyr-realm-aliases,
;;;               zephyr-extra-subscriptions), flush undo after inserting 
;;;               incoming msg, so user doesn't accidentally undo msg insertion
;;; 25 Feb 1994 - (2.2-beta7) zephyr-new-buffer won't put duplicates on 
;;;               the zephyr-hook-list, documented zephyr-mode-hook
;;; 23 Feb 1994 - (2.2-beta6) replace zephyr-{ignore,subscribe}-temporarily w/
;;;               zephyr-{ignore,listen}-{instance,sender}-temporarily,
;;;               make default forever
;;; 23 Feb 1994 - (2.2-beta5) zephyr-ignore-temporarily only affects 
;;;               non-personal zgrams now
;;; 21 Feb 1994 - (2.2-beta4) added signature field for zephyr-filters,
;;;               added zephyr-version function, 
;;; 19 Feb 1994 - (2.2-beta3) if tzc claims it's been cut off, restart.
;;; 16 Feb 1994 - (2.2-beta2) made to-list parsing a bit smarter
;;;               to avoid matching some things that aren't to-lists,
;;;               fixed log-buffer truncation so huge messages work,
;;;               changed default location of tzc to /afs/cs/misc/others,
;;;               disabled zephyr-get-message-info (memory pig?)
;;; 12 Feb 1994 - (2.2-beta1) added zephyr-after-send-hook,
;;;               disallow parens within (outgoing) instances (so that,
;;;               e.g., "joe(foo),jane(bar)" works).
;;;  8 Feb 1994 - added zephyr menu on menubar & popup, with history
;;;               as a submenu; added zephyr-subscribe-temporarily and
;;;               zephyr-ignore-temporarily;
;;;               added zephyr-history-ignore-regexp variable
;;;  6 Feb 1994 - added zephyr-cutoff to interpret the (as yet unimplemented)
;;;               message from tzc saying it's been cut off from the server.
;;;  1 Feb 1994 - changed zephyr-instance-xlate-hook to 
;;;               zephyr-instance-xlate-function, changed 
;;;               zephyr-send-hook to zephyr-send-function,
;;;               added zephyr-initial-history variable,
;;;               ignore case when examining class names
;;; 16 Nov 1993 - moved zephyr-notify-with-* to zephyr-insert-hook-list,
;;;               fixed zephyr-send to strip text-property stuff in fsf19,
;;;               define zephyr-mode-map when this file is loaded,
;;;               fix up test for running zephyr process in (zephyr-mode)
;;;  6 Nov 1993 - zephyr-{next,previous}-destination (M-p, M-n) smarter.
;;;               minor zephyr-receive-sentinel change for remote-tzc.
;;;               corrected doc for zephyr-delete-messages-from
;;;  3 Nov 1993 - parse the [larry moe curly] to-lists for the history
;;;               list (see zephyr-touch, zephyr-send-show-multiple).
;;;               put zephyr-running-lemacs const in.
;;;  1 Nov 1993 - added zephyr-insert-hook-list variable (see documentation),
;;;               changed default format of PING notifications,
;;;               and don't downcase instance names in minibuffer
;;;               notifications except for instance PERSONAL.
;;; 13 Oct 1993 - minor fixes for epoch compatibility
;;; 19 Sep 1993 - (version 2.1) fixed minor bug in zephyr-get-message-info
;;;               (but it's still a bit buggy), created zephyr-version const,
;;;               changed (FORGED) & (NOAUTH) to [FORGED] & [NOAUTH],
;;;               fixed zephyr-add-timestamp to use the real timestamp
;;;               as supplied by tzc, removed running-lemacs definition,
;;;               added instance info to PING notifications,
;;;               created zephyr-popup-menu (bound to button3 by default)
;;; 26 Jul 1993 - fix zephyr-previous-names so the history ordering
;;;               is more reasonable
;;; 22 Jul 1993 - added zephyr-get-message-info function (lemacs only)
;;;               which shows detailed info on the received message
;;;               at point
;;; 21 Jun 1993 - added completion on zephyr-compose from 
;;;               zephyr-previous-names
;;;  9 Apr 1993 -
;;;     - changed format for LOGIN class to print "login" or "logout"
;;;       as appropriate  
;;;     - fixed bug in zephyr-insert
;;;     - added zephyr-recipients-{ignore,accept,always-accept}-regexp,
;;;       zephyr-opcodes-{ignore,accept}-list.  Selecting on recipient
;;;       is the best way to get personal zgrams only
;;;     - added zephyr-beep-personal-only
;;;     - added ping notification by minibuffer message (if the opcode
;;;       filters let them through)
;;;     - added C-c C-u to erase to beginning of current zgram,
;;;       M-a to skip to beginning of message (zephyr-kill-input,
;;;       zephyr-goto-beginning-of-input)
;;;     - added support for lucid-emacs (see zephyr-receive-face and
;;;       zephyr-deiconify-on-receive)
;;;     - added zephyr-auto-fill-on-send to eliminate long final line of msg.
;;;     - changed default zephyr-signature to be full name
;;;     - added zephyr-receive-divider-regexp, which must be set to
;;;       match zephyr-receive-divider if the latter is changed
;;;     - added zephyr-send-whole-message, default nil.  if t, then
;;;       send the entire message up to the next divider (not just up
;;;       to point).
;;;     - zephyr-send will give an error if point seems to be in a
;;;       received message
;;;     - extra documentation on zephyr-receive-program about kauthd
;;;     - changed default for zephyr-receive-program to use
;;;       /afs/cs/user/dkindred/bin/tzc
;;;     - changed zephyr-restart-receiver to send SIGINT rather than
;;;       SIGKILL, so tzc shutdown activity can take place.
;;;     - do log-buffer trimming before inserting a new tzc response
;;;       (not after) so we don't fail to parse big messages
;;;  2 Mar 1993 - spaces are now possible in instance names
;;; 15 Feb 1993 - added zephyr-send-hook and zephyr-send-show-multiple
;;; 21 Jan 1993 - removed zwgc code to emulate tzc because it can't
;;;               send messages (handle the input).  added a
;;;               save-excursion to zephyr-do-insert so that point isn't
;;;               moved in buffers that aren't visible when they receive 
;;;               a message.  rewrote zephyr-notify-with-scroll so it
;;;               doesn't scroll the selected window.
;;;  7 Jan 1993 - replaced ` with cons due to incompatible versions of
;;;               ` floating around.
;;;  1 Jan 1993 - ugh, the recipient of "(foo)" is now "".
;;;               zephyr-make-text-recipient now handles "foo(bar)".  
;;;               and handle the additional information in "to" tag of
;;;               tzcspew sent messages. added * to docstring of a bunch
;;;               of variables.  removed zephyr-require-tzcspew. when 
;;;               receiving a message, formats as "foo(bar)", rather 
;;;               than "foo (bar)"
;;; 13 Dec 1992 - parse names a little differently:  "foo" now goes to
;;;               the user foo.  "(foo)" goes to everyone reading instance
;;;               foo, and "foo(bar)" goes to just user foo as instance bar.
;;;               aliases are now recursive.
;;; 13 Dec 1992 - merged in changes to handle new tzc, run tzc with
;;;               pipe rather than pty.  call this 2.0 because the new
;;;               tzc is slightly incompatible.
;;;  7 Dec 1992 - handles class MESSAGE messages that have the wrong
;;;               number of fields, as can happen if the sender embeds a NULL.
;;; 16 Nov 1992 - removed old code to parse output of vanilla zwgc
;;; 24 Oct 1992 - if any function in the hook list returns nil, then do
;;;               not call the rest of the hooks.
;;; 22 Aug 1992 - added sentinel so when zephyr receiver dies, we are
;;;               notified.  remove sentinel before restarting.
;;; 29 Jul 1992 - added zwgc description file that emulates tzc
;;; 26 Jul 1992 - fixed zephyr-add-timestamp, it wasn't returning the msg
;;; 11 Jul 1992 - fixed: M-p and M-n raise error if no messages sent yet.
;;;               split zephyr-notify into zephyr-notify-with-beep,
;;;               zephyr-notify-with-message, and zephyr-notify-with-scroll.
;;;               added zephyr-touch.  calls zephyr-touch from zephyr-insert
;;;               (rather than notify).
;;;  9 Jul 1992 - replaced a bunch of random hooks with zephyr-hook-list,
;;;               including reorganizing the code, various small improvements.
;;;               commented out dangling encryption stuff.  tzc now stamps 
;;;               with date string rather than integer so that it is human
;;;               readable in the log, and also cuz emacs ints aren't big
;;;               enuf. added todd's delete-messages function.  
;;;               removed that bury crap.

(defconst zephyr-version (purecopy "2.5.19")
  "Version number of this version of zephyr.el")

(defun zephyr-version ()
  (interactive)
  (message "zephyr.el, version %s" zephyr-version))

(defconst zephyr-running-xemacs 
  (and (string-match "XEmacs\\|Lucid" emacs-version) t)
  "are we running XEmacs?")

(defconst zephyr-running-fsf19
  (and (not zephyr-running-xemacs)
       (boundp 'emacs-major-version)
       (= emacs-major-version 19))
  "are we running Emacs 19?")

(defconst zephyr-running-fsf20
  (and (not zephyr-running-xemacs)
       (boundp 'emacs-major-version)
       (= emacs-major-version 20))
  "are we running Emacs 20?")

(defconst zephyr-running-fsf23
  (and (not zephyr-running-xemacs)
       (boundp 'emacs-major-version)
       (= emacs-major-version 23))
  "are we running Emacs 23?")

(defconst zephyr-running-recent-fsf
  (or zephyr-running-fsf19 zephyr-running-fsf20 zephyr-running-fsf23)
  "are we running Emacs 19 or greater?")

(defvar zephyr-filters '(((opcode . "PING") nil))   ; ignore PINGs
  "*This list determines which incoming zephyrs will be inserted in 
the current buffer.  Each filter on the list is of the form
   ((FIELD1 . REGEXP1) (FIELD2 . REGEXP2) ... ACCEPT [TIMEOUT])
where FIELD is 'instance, 'class, 'opcode, 'recipient, 'sender, 'signature, 
               or 'body;
      REGEXP is a regular expression matching part or all of the field's value;
      ACCEPT is t to accept the message, nil to ignore it, or
                the name of a function which, when applied to the message,
                will return t, nil, or 'pass (meaning ignore this filter).
      TIMEOUT is a time after which this entry is ignored.  The format
              is a list of three integers, as returned by (current-time).
An incoming zephyrgram is matched against each element of the list in
sequence until one matches, at which point the value of ACCEPT determines
whether the zephyr is ignored or accepted.  If there is no match for the
zephyrgram, it is accepted.  Note that the regexp may match any part of
the field, so if you want to match a whole (single-line) field, use
something like \"^foo$\".

The value of zephyr-filters is local to each zephyr buffer, so it is 
best set from a function in the zephyr-mode-hook, or after calling
zephyr-new-buffer.")

;; ;;; THESE IGNORE/ACCEPT VARIABLES ARE OBSOLETE.  USE zephyr-filters INSTEAD.
;; 
;; (defvar zephyr-senders-ignore-regexp nil
;;   "THIS VARIABLE IS OBSOLETE.  See zephyr-filters instead.
;; 
;; if this regexp matches the sender, then ignore the message.  it's
;; typical to put yourself in this list so that you don't get what you
;; send to instances you also read.  to ignore nobody, use nil.")
;; 
;; (defvar zephyr-senders-accept-regexp nil
;;   "THIS VARIABLE IS OBSOLETE.  See zephyr-filters instead.
;; 
;; only accept messages from users who match this regexp.  nil means
;; match everybody.")
;; 
;; (defvar zephyr-instances-ignore-regexp nil
;;   "THIS VARIABLE IS OBSOLETE.  See zephyr-filters instead.
;; 
;; if this regexp mathces the instance, then ignore the message. use
;; nil to ignore nobody.")
;; 
;; (defvar zephyr-instances-accept-regexp nil
;;   "THIS VARIABLE IS OBSOLETE.  See zephyr-filters instead.
;; 
;; only accept messages from instances that match this regexp.  nil
;; means match all instances")
;; 
;; (defvar zephyr-recipients-ignore-regexp nil
;;   "THIS VARIABLE IS OBSOLETE.  See zephyr-filters instead.
;; 
;; if this regexp matches the recipient, then ignore the message. use
;; nil to ignore nothing.  the recipient will generally be your userid
;; ;; or the empty string")
;; 
;; (defvar zephyr-recipients-accept-regexp nil
;;   "THIS VARIABLE IS OBSOLETE.  See zephyr-filters instead.
;; 
;; only accept messages for recipients matching this regexp.  nil
;; means match all recipients.  to receive personal zephyrgrams only, put
;; your userid here.  the recipient will generally be your userid or
;; the empty string")
;; 
;; (defvar zephyr-recipients-always-accept-regexp
;;   (concat "^" (user-login-name) "$")
;;   "THIS VARIABLE IS OBSOLETE.  See zephyr-filters instead.
;; 
;; accept messages to this recipient regardless of whether they match
;; your other accept- and ignore- regexps.  (the zephyr-opcodes-ignore-list
;; and zephyr-opcodes-accept-list are still obeyed.)  this should
;; probably be set to something that matches your username, and doesn't
;; match the empty string.")
;; 
;; (defvar zephyr-opcodes-ignore-list '(PING)
;;   "THIS VARIABLE IS OBSOLETE.  See zephyr-filters instead.
;; 
;; if the opcode is in this list, then ignore the message. use nil
;; to ignore nothing.  common values of opcode are 'PING and nil.")
;; 
;; (defvar zephyr-opcodes-accept-list nil
;;   "THIS VARIABLE IS OBSOLETE.  See zephyr-filters instead.
;; 
;; only accept messages with opcodes in this list. use nil to 
;; match everything.  common values of opcode are 'PING and nil.")
;; 
(defvar zephyr-extra-subscriptions nil
  "*A list of extra subscriptions to request from tzc.  By default, 
tzc subscribes to all broadcast zephyrgrams of class MESSAGE 
in the local realm.  Each subscripition is a 3-item list:
     (class instance recipient)
in the style of .zephyr.subs.  For example, if you want to subscribe to
broadcast zgrams (of class MESSAGE) in the ANDREW.CMU.EDU realm, do
 (setq zephyr-extra-subscriptions '((\"MESSAGE\" \"*\" \"*@ANDREW.CMU.EDU\")))
The new subscriptions will take effect the next time the client 
is (re)started.  

Note: this feature requires tzc version 2.4 or later.")


(defvar zephyr-mode-hook nil
  "Function or functions called when a zephyr buffer is created,
after the receiver process is started.  The current buffer is the 
new zephyr buffer.")

(defvar zephyr-hook-list
  '(zephyr-parse
    zephyr-dispatch
    zephyr-decode-fields
    zephyr-parse-fields
    zephyr-add-banner
    zephyr-insert)
  "*A list of functions to call to process an incoming message.  A lot
of customization can be done by changing this list.  For example, to
visibly timestamp messages, add the function zephyr-add-timestamp next
to zephyr-add-banner.  This list forms a pipeline; each function takes 
a message as its argument and returns a message as its result.  If a 
function returns nil, processing stops.

These functions are called once per message.  To run functions once 
for each buffer in which the message is inserted, see
zephyr-insert-hook-list.")

(defvar zephyr-insert-hook-list
  '(zephyr-ping-notify
    zephyr-insert-message
    zephyr-make-message-extents
    zephyr-touch
    zephyr-notify-with-message
    ;; zephyr-notify-with-scroll
    zephyr-notify-with-deiconify
    zephyr-notify-with-beep)
"*A list of functions to call to insert a message in a buffer.  These
functions are called by zephyr-insert if the message passes the zephyr-filters
for a given zephyr buffer.  Each function must return a (possibly modified)
message for the following functions to use.  The list of functions will be 
called for each buffer in which the message is to be inserted.")

(defvar zephyr-send-divider "<<< "
  "pattern to mark the beginning of the message to be sent.
everything between it and point is considered part of the message.
everything between it and the beginning of the line is considered a
list of recipients.
if you set this, be sure to set zephyr-send-divider-regexp appropriately.")

(defvar zephyr-send-divider-regexp "<<<\\( \\|$\\)"
  "regexp to match zephyr-send-divider.  it pays to be a bit lenient.")

(defvar zephyr-receive-divider "» "
  "string that appears immediately before the message body of incoming
zephyr-grams.  
if you set this, be sure to set zephyr-receive-divider-regexp appropriately.")

(defvar zephyr-receive-divider-regexp "»\\( \\|$\\)"
  "regexp that matches zephyr-receive-divider.")

(defvar zephyr-send-whole-message nil
  "*if nil, zephyr-send-and-compose will send the message up to point;
otherwise, it will send the entire message at point.")

(defvar zephyr-receive-program
  (list 
   (cond ((file-exists-p "/usr/local/bin/tzc") "/usr/local/bin/tzc")
	 ((file-exists-p "/afs/cs.cmu.edu/misc/others/@sys/omega/bin/tzc" )
	  "/afs/cs.cmu.edu/misc/others/@sys/omega/bin/tzc")
	 (t "tzc"))
   "-e" "NET-ANNOUNCED")
  "*list containing program and its options.  invoked to receive
zephyr-grams.  CMU-CS note: if you use Bennet Yee's kauthd, you should
probably append
        '(\"-p\" (expand-file-name \"~/.tzc-pid\"))
to this variable, invoke kauthd with the -r flag, and include
        if (-e ~/.tzc_pid) kill -USR1 `cat ~/.tzc_pid`
or the equivalent in your ~/.kauthd.userid@CS.CMU.EDU file.")

(defvar zephyr-process-connection-type nil
  "The type of connection used to communicate with zephyr-receive-program.
Ordinarily you should not have to set this, but Bennet Yee's tzc-remote
seems to require this to be set to t.  See the documentation for the 
process-connection-type variable for possible values.")

(defvar zephyr-receive-face nil
  "*XEmacs face in which incoming messages will be displayed,
   or nil to use the default face for the buffer")

(defvar zephyr-kerberos-authenticate t
  "*if true, then the client is requested to use kerberos
authentication when sending messages.  othewise use no authentication.")

(defvar zephyr-aliases '(("stooges" . ("larry" "curly" "moe")))
  "*alist of recursive aliases.")

(defvar zephyr-realm-aliases '(("ANDREW" . "ANDREW.CMU.EDU")
			       ("CS" . "CS.CMU.EDU")
			       ("AGRABAH" . "AGRABAH.CS.CMU.EDU")
			       ("DEMENTIA" . "DEMENTIA.CMU.EDU")
			       ("ATHENA" . "ATHENA.MIT.EDU"))
  "*alist of aliases for zephyr/kerberos realms")

(defvar zephyr-initial-history '("nobody")
  "*List of strings which is used as the initial history when a new
zephyr buffer is created.  The last name on this list will be the
initial destination when the buffer is created.")

(defvar zephyr-history-ignore-regexp 
  (mapconcat
   '(lambda (x) x)
   '(
     "^(facilities\\.announce)$"
     "^root(print_job)$"
     "^(plusplus:.*)$"
     "^(login:.*)$"
     "^(LOGIN:.*)$"
     "^plusplus$"
     "^zarchive$"
     "^zbot$"
     "^silverberg$"
     "^anonymoose$"
     "^zarchive(zephyr-history-response)$"
     "(mail_delivery)$"			; any sender
     )
   "\\|")
  "*if a recipient(instance) string matches this regexp, don't add it 
to the history (to prevent accidents).")

(defvar zephyr-instance-xlate-function 'zephyr-instance-xlate
  "call this function with each recipient.  returns a list of options
for the send program.  typically, it recognizes instances.")

(defvar zephyr-signature nil
  "*signature to use on outgoing zephyrgrams")

(defvar zephyr-unauth-string "[NOAUTH]"
  "string that appears next to messages that are from unauthenticated
sources.")

(defvar zephyr-failed-string "[FORGED]"
  "string that appears next to messages whose authentication attempt
failed.")

(defvar zephyr-lazy-beep-time 120
  "*beep, unless a msg has been received since this many seconds ago.
if this is nil, then never beep.")

(defvar zephyr-beep-personal-only nil
  "*if non-nil, only beep for messages whose recipient matches
zephyr-id.")

(defvar zephyr-deiconify-on-receive nil
  "*In XEmacs, pop up the dedicated screen of a zephyr buffer 
when it receives a message.  You can use make-local-variable
to make this property buffer-specific.  See 
set-buffer-dedicated-frame.")

(defvar zephyr-warn-empty-message t
  "*If non-nil, warn the user before sending a whitespace-only message.")

(defvar zephyr-send-function 'zephyr-send
  "* call this function to send the msg to the subprocess.  to
automatically indicate multiple recipients, use
zephyr-send-show-multiple.")

(defvar zephyr-before-send-hook nil
  "*Function or functions to run just before sending a zephyrgram.  The current
buffer will be the zephyr buffer from which the message was sent, and point
will be where it was when the user invoked zephyr-send-and-compose (within 
the message body).  The function(s) will be passed two arguments: 
the text of the message, and a list of (instance . recipient) pairs,
where recipient is \"\" for broadcast zephyrs.
See also zephyr-after-send-hook.") 

(defvar zephyr-after-send-hook nil
  "*Function or functions to run just after sending a zephyrgram.  The current
buffer will be the zephyr buffer from which the message was sent, and point
will be where it was when the user invoked zephyr-send-and-compose (within 
the message body).  The function(s) will be passed two arguments: 
the text of the message, and a list of (instance . recipient) pairs,
where recipient is \"\" for broadcast zephyrs.
See also zephyr-before-send-hook.") 

(defvar zephyr-auto-fill-on-send nil
  "*If auto-fill-mode is on in the zephyr buffer and this variable
is non-nil, then do auto-filling before sending a message.")

(defvar zephyr-log-buffer-limit 32000
  "*prevent the zephyr-log-buffer from expanding beyond this many
characters.  the buffer acts as a queue -- text from the beginning is
thrown away as new messages arrive.  the value nil means that the
buffer will grow without bound.")

(defvar zephyr-buffer-limit 32000
  "*prevent the *zephyr* buffer from expanding beyond this many
characters.  the buffer acts as a queue -- text from the beginning is
thrown away as new messages arrive.  the value nil means that the
buffer will grow without bound.")

(defvar zephyr-client-eom-string "\0"
  "string matching the end of the output from the zephyr client
receiving messages.  for zwgc this is usually ^G.")

(defvar zephyr-client-bom-string "\1"
  "string matching the beginning of the output from the zephyr client
receiving messages.  must be different from
zephyr-client-eom-string.")

(defvar zephyr-id nil
  "Your zephyr userid.  This will be updated when tzc starts.")

(defvar zephyr-realm nil
  "Your default zephyr realm.  This will be updated when tzc starts.")

(defvar zephyr-mode-map nil
  "Keymap used in zephyr mode.")

;;; set up the zephyr-mode-map
(if (null zephyr-mode-map)
    (progn
      (setq zephyr-mode-map (make-sparse-keymap))
      (define-key zephyr-mode-map "\C-j" 'zephyr-send-and-compose)
      (define-key zephyr-mode-map "\C-c?" 'describe-mode)
      (define-key zephyr-mode-map "\C-c\C-s" 'zephyr-compose)
      (define-key zephyr-mode-map "\C-c\C-r" 'zephyr-restart-receiver)
      (define-key zephyr-mode-map "\C-c\C-c" 'zephyr-send-and-compose)
      (define-key zephyr-mode-map "\C-c\C-u" 'zephyr-kill-input)
      (define-key zephyr-mode-map "\C-c\C-y" 'zephyr-quote-message)
      (define-key zephyr-mode-map "\ea" 'zephyr-goto-beginning-of-input)
      (define-key zephyr-mode-map "\ep" 'zephyr-previous-destination)
      (define-key zephyr-mode-map "\en" 'zephyr-next-destination)
      (define-key zephyr-mode-map "\C-c\C-d" 'zephyr-delete-messages-from)
      (if zephyr-running-xemacs
        ;; This isn't necessary in fsf19, as C-down-mouse-3 is by
        ;; default bound to mouse-major-mode-menu, which is the same
        ;; thing as the below
        (define-key zephyr-mode-map 'button3 'zephyr-popup-menu))))

; (defvar zephyr-encrypt-program "crypt"
;   "invoke this program to encrypt messages.")

; (defvar zephyr-decrypt-program "crypt"
;  "invoke this program to decrypt messages.")

; (defvar zephyr-cypher-is-binary t
;   "true if the cypher-text generated by zephyr-encrypt-program is
; binary.")

; (defvar zephyr-passwords '(("an-encrypted-instance" .
; 			    "some-password"))
;   "alist of instance-password pairs.  if the password is nil, the user
; is prompted when it is needed.  the passwords can be cleared with
; zephyr-clear-passwords. encrpytion doesn't work.")

; how to remove buffers when they are killed?
(defvar zephyr-buffer-list nil
  "list of buffers that are receiving zephyrgrams")

(defvar zephyr-log-buffer nil)
(defvar zephyr-log-buffer-name "*log-zephyr*")
(defvar zephyr-process nil)
(defvar zephyr-last-zgram-timestamp nil)
(defvar zephyr-lazy-beep-last nil)
(defvar zephyr-lecture-given nil)

;;;###autoload
(defun zephyr-new-buffer (&optional name no-prefix)
  "create a new buffer for reading and sending zephyrgrams.  the
buffer is named *zephyr-NAME*, where NAME is printed representation of
the argument, or just *zephyr* if NAME is nil, or not provided.
If NO-PREFIX is non-nil, the given name will be used as is."
  (interactive)
  (let ((name (if name
		  (if no-prefix name (concat "*zephyr-" name "*"))
		"*zephyr*")))
    (switch-to-buffer name)
    (zephyr-mode)
    (zephyr-compose "")
    ;; don't put the same buffer on the list twice.
    ;; might make more sense to use generate-new-buffer instead of 
    ;; switch-to-buffer so we never get into this situation
    (if (not (memq (current-buffer) zephyr-buffer-list))
	(setq zephyr-buffer-list (cons (current-buffer)
				       zephyr-buffer-list)))
    (current-buffer)
    ))

(defun zephyr-match (s n)
  (substring s
	     (match-beginning n)
	     (match-end n)))
  
;;; convert a name like "joe(cls:inst)" to a list of class/instance/user lists
(defun zephyr-instance-xlate (name)
  (let* ((l (assoc name zephyr-aliases)) ; not quite right: alias(inst) breaks
	 (n (string-match "(\\(.*\\))" name))
	 (classinst (if n 
			(zephyr-match name 1)
		      nil)))
    (cond (l (apply 'append (mapcar 'zephyr-instance-xlate (cdr l))))
	  (n (let* ((cls   (or (zephyr-extract-class classinst) "MESSAGE"))
		    (inst-rlm (zephyr-extract-instance classinst))
		    (recip-inst (if (= n 0)
				    (if (string-match "@[^@]*$" inst-rlm)
					(let ((p (match-beginning 0)))
					  (cons (zephyr-unalias-realm
						 (substring inst-rlm p))
						(substring inst-rlm 0 p)))
				      (cons "" inst-rlm))
				  (cons (zephyr-unalias-realm
					 (substring name 0 n))
					inst-rlm))))
	       (list (list cls (cdr recip-inst) (car recip-inst)))))
	  (t (list (list "MESSAGE" "PERSONAL" (zephyr-unalias-realm name)))))))

(defun zephyr-extract-class (s)
  (if (string-match "^\\([^:]*\\):" s)
      (zephyr-match s 1)
    nil))

(defun zephyr-extract-instance (s)
  (if (string-match "^[^:]*:" s)
      (substring s (match-end 0))
    s))

(defun zephyr-class-instance-string (class inst)
  (let ((c    (zephyr-make-string class)))
    (if (string-equal (upcase c) "MESSAGE")
	inst
      (concat c ":" inst))))

;;; convert strings like "foo@ANDREW" to "foo@ANDREW.CMU.EDU" using
;;; zephyr-realm-aliases
(defun zephyr-unalias-realm (s)
  (if s
      (let ((n (string-match "@\\(.*\\)" s)))
	(if n
	    (let* ((r       (upcase (zephyr-match s 1)))
		   (r1      (assoc r zephyr-realm-aliases))
		   (realm   (if r1 (cdr r1) r)))
	      (format "%s@%s" (substring s 0 n) realm))
	  s))
    nil))
  

;;; convert strings like "foo@ANDREW.CMU.EDU" to "foo@ANDREW" using
;;; zephyr-realm-aliases
(defun zephyr-alias-realm (s)
  (if s
      (let ((n (string-match "@\\(.*\\)" s)))
	(if n
	    (let* ((r       (upcase (zephyr-match s 1)))
		   (r1      (let ((l zephyr-realm-aliases))
			      (while 
				  (and l (not (string= (cdr (car l)) r)))
				(setq l (cdr l)))
			      (car l)))
		   (realm   (if r1 (car r1) r)))
	      (format "%s@%s" (substring s 0 n) realm))
	  s))
    nil))

;;; convert string to list of strings, basically separate it into
;;; words.  space, tab, and comma are considered separators.
;;; no parens are allowed within instances (so we can send a zephyr
;;; to, e.g., "joe(foo),jane(bar)")
(defun zephyr-send-make-recipient-list (recipient)
  (if (string-match "[ \t,]*\\([^ \t,]*([^()]+)\\|[^ \t,]+\\)" recipient)
      (let* ((beg (match-beginning 1))
	     (end (match-end 1))
	     (name (substring recipient beg end)))
	(append (funcall zephyr-instance-xlate-function name)
		(zephyr-send-make-recipient-list
		 (substring recipient end))))
    nil))

;;; (defun zephyr-get-instance (l)
;;;   (if l
;;;       (if (equal (car l) "-i")
;;; 	  (car (cdr l))
;;; 	(zephyr-get-instance (cdr l)))
;;;     nil))

(defvar zephyr-last-recipient nil
  "Do not set this.  If you want to set the default first recipient, see
zephyr-initial-history.")

(defvar zephyr-class-and-opcode-symbols t
  "For backwards/forwards compatibility.
If non-nil, force class and opcode names to be symbols.
If nil, force class and opcode names to be strings.")

;;; XXX this is a kludge--should be merged with the one below
(defun zephyr-make-text-recipient-class (cls-inst-recip)
  (let ((class (nth 0 cls-inst-recip))
	(inst  (nth 1 cls-inst-recip))
	(recip (nth 2 cls-inst-recip)))
    (if (or (= (length recip) 0) (eq ?@ (aref recip 0)))
	; broadcast
	(concat "(" (zephyr-class-instance-string class inst) recip ")")
      ; personal
      (if (and (string-equal inst "PERSONAL")
	       (string-equal class "MESSAGE"))
	  recip
	(concat recip "(" (zephyr-class-instance-string class inst) ")")))))

(defun zephyr-make-text-recipient (recip)
  (cond ((string-equal (car recip) "PERSONAL")
	 (cdr recip))
	(t (concat (cdr recip) "(" (car recip)")"))))

;;; if zephyr-auto-fill-on-send and we're in auto-fill-mode, then
;;; do auto-fill before sending
;;; (GNU v19 and Lemacs v19 use auto-fill-function.  v18 uses auto-fill-hook
(defun zephyr-maybe-do-auto-fill ()
  (if zephyr-auto-fill-on-send
      (let ((af-func  (cond 
		       ((boundp 'auto-fill-function) auto-fill-function)
		       ((boundp 'auto-fill-hook) auto-fill-hook)
		       (t nil))))
	(if (and af-func (> (current-column) fill-column))
	    (funcall af-func)))))

(defun zephyr-global-broadcast-p (recip)
  (let ((class (nth 0 recip))
	(inst  (nth 1 recip))
	(dest  (nth 2 recip)))
    (cond
     ((and (string-equal (upcase class) "MESSAGE")
	  (or (and (string-equal dest "")
		   (string-equal zephyr-realm "ECE.CMU.EDU"))
	      (string-equal dest "@ECE.CMU.EDU")))
      ; it's a CS broadcast
      (cond
       ((string-equal (downcase inst) "facilities.announce")
	"all CS users")
       ((string-equal (downcase inst) "test.forbidden")
	"a test forbidden instance")
       ((string-match "^anon[-.]" (downcase inst))
	(concat "everybody, NOT anonymously"))
       ))
     (t 
      nil))))

(defun zephyr-check-message (msg recip-list)
  (and (not (and zephyr-warn-empty-message
	    (string-match "^[ \t\n]*"  msg)
	    (= (match-end 0) (length msg))
	    (not (y-or-n-p "Really send an empty message? "))))
       (let ((l recip-list)
	     (ok t))
	 (while (and l ok)
	   (let ((whom (zephyr-global-broadcast-p (car l))))
	     (if whom
		 (setq ok (and ok
			       (y-or-n-p 
				(format "Really send this message to %s? "
					whom))))))
	   (setq l (cdr l)))
	 ok)))

(defun zephyr-send-and-compose ()
  "send the zephyrgram before point, and start composing another one."
  (interactive)
  (zephyr-maybe-do-auto-fill)
  (let* ((msg-and-recip  (zephyr-send-get-msg-and-recip))
	 (msg            (car msg-and-recip))
	 (recipient      (cdr msg-and-recip))
	 (recip-list     (zephyr-send-make-recipient-list recipient))
	 (text-recip-list (mapconcat 'zephyr-make-text-recipient-class
				     recip-list " ")))
    (if (zephyr-check-message msg recip-list)
	(progn
	  (unwind-protect
	      (let* ((cur-buf (current-buffer))
		     (log-buf zephyr-log-buffer)
		     (log-buffer-size (progn (set-buffer log-buf)
					     (buffer-size))))
		(set-buffer cur-buf)
		(save-excursion
		  (message "Sending to %s..." text-recip-list)
		  (setq zephyr-last-recipient recipient)
		  (zephyr-touch-name recipient)
		  ;; (let* ((inst (zephyr-get-instance recip-list))
		  ;;   (a (assoc inst zephyr-passwords)))
		  ;; (if a (progn
		  ;;      (setq msg (zephyr-en/decrypt msg (cdr a) t))
		  ;;    (if zephyr-cypher-is-binary
		  ;;  (setq msg (zephyr-btoa msg))))))
		  (zephyr-run-hook-with-args 'zephyr-before-send-hook
					     msg recip-list)
		  (funcall zephyr-send-function msg recip-list))
		(zephyr-run-hook-with-args 'zephyr-after-send-hook 
					   msg recip-list)
		(zephyr-limit-buffer-size zephyr-buffer-limit)
		(set-buffer log-buf)
		(if (not (equal log-buffer-size (buffer-size)))
		    (progn
		      (pop-to-buffer log-buf)
		      (error "tzc complained")))
		(set-buffer cur-buf)))
	  ;; this is protected, so in case of error sending, we still set up
	  ;; for the next one
	  (zephyr-compose ""))
      (message "Message NOT sent to anyone."))))
  
(defun zephyr-old-send-get-msg-and-recip ()
  (save-excursion
    (let* ((end-msg (point))
	   (pat (concat "^.+\\("
			zephyr-send-divider-regexp
			"\\)"))
	   (end-recipient (progn (re-search-backward pat)
				 (match-beginning 1)))
	   (start-recipient (match-beginning 0))
	   (start-msg (match-end 0))
	   (msg (zephyr-buffer-substring-no-properties start-msg end-msg))
	   (recipient (zephyr-buffer-substring-no-properties 
		       start-recipient end-recipient)))
      (cons msg recipient))))

(defun zephyr-send-get-msg-and-recip ()
  (save-excursion
    (let* ( ; (send-point (progn (end-of-line) (point)))
	   (either-div (concat "^.+\\(" zephyr-receive-divider-regexp
			       "\\|" zephyr-send-divider-regexp "\\)"))
	   (send-div   (concat "^.+\\("
			       zephyr-send-divider-regexp
			       "\\)"))
	   (end-msg    (if (re-search-forward either-div (point-max) t)
			   (match-beginning 0)
			 (point-max)))
	   (start-recip-ck  (progn 
			      (goto-char end-msg)
			      (if (re-search-backward either-div (point-min) t)
				  (match-beginning 0)
				(error "no message at point"))))
	   (start-recip     (progn (goto-char end-msg)
				   (re-search-backward send-div (point-min) t)
				   (match-beginning 0)))
	   (--              (if (not (= start-recip start-recip-ck))
				(error "point is in a received message")))
	   (end-recip      (match-beginning 1))
	   (start-msg (match-end 0))
	   (msg (zephyr-buffer-substring-no-properties start-msg end-msg))
	   (recipient (zephyr-buffer-substring-no-properties
		       start-recip end-recip)))
      (cons msg recipient))))
	 
(defun zephyr-send-show-multiple (msg to-list)
  (zephyr-send
   (if (< 1 (length to-list))
       (concat "[" (mapconcat 'zephyr-make-text-recipient-class
			      to-list " ")
	       "]\n" msg)
     msg)
   to-list))


(if (fboundp 'encode-coding-string)
    (fset 'zephyr-encode-coding-string (symbol-function 'encode-coding-string))
  (defun zephyr-encode-coding-string (x y &optional z) x))
(if (fboundp 'decode-coding-string)
    (fset 'zephyr-decode-coding-string (symbol-function 'decode-coding-string))
  (defun zephyr-decode-coding-string (x y &optional z) x))

;;; MSG is a string, the body of the message to be sent
;;; TO-LIST is a list, each of whose elements is a 3-element list: 
;;;     class/instance/recipient
;;; The signature is taken from the variable zephyr-signature.
;;; Example:
;;;   (zephyr-send "HI" '(("MESSAGE" "PERSONAL" "bovik") ("MESSAGE" "foo" "")))
(defun zephyr-send (msg to-list)
  ;; XXX this is a bit hairy because the current tzc interface
  ;;     treats class differently from instance, for backward-compatibility.
  (while to-list
    (let ((class (nth 0 (car to-list)))
	  (recips nil))
      (while (and to-list (string-equal (nth 0 (car to-list)) class))
	(setq recips (cons (cons (zephyr-encode-coding-string 
				  (nth 1 (car to-list)) 'ctext)
				 (nth 2 (car to-list)))
			   recips))
	(setq to-list (cdr to-list)))
      (setq msg (zephyr-encode-coding-string msg 'ctext))
      (zephyr-send-to-client  (list '(tzcfodder . send)
				    (cons 'auth zephyr-kerberos-authenticate)
				    (cons 'class class)
				    (cons 'recipients recips)
				    (cons 'message
					  (list 
					   (zephyr-encode-coding-string 
					    zephyr-signature 'ctext) msg)))))))

;; for fsf19, this strips out text-property information from strings,
;; so we can use (format "%S" ...) on the object without getting text-prop info
;; (works recursively on conses, but not on vectors).
(defun zephyr-strip-text-properties (obj)
  (cond ((stringp obj)
	 (format "%s" obj))
	((consp obj)
	 (cons (zephyr-strip-text-properties (car obj))
	       (zephyr-strip-text-properties (cdr obj))))
	(t
	 obj)))

(defun zephyr-send-to-client (obj)
  "send the elisp object OBJ to the tzc process."
  (let* ((obj1   (if (or zephyr-running-xemacs
                       (not zephyr-running-recent-fsf))
		     obj
		   (zephyr-strip-text-properties obj)))
	 (str (format (if (or (and (boundp 'epoch::version) epoch::version)
			      (and (not zephyr-running-recent-fsf)
				   (not zephyr-running-xemacs)))
			  "%s\n"	   ; version 18 & Epoch need this
			"%S\n")     ; FSF v19 and xemacs need this
		      obj1)))
    ;; (print str (get-buffer "*scratch*"))
    (process-send-string zephyr-process str)))

;;; go to the end of the buffer, add a new header
(defun zephyr-send-setup (recipient)
  (goto-char (point-max))
  (if (not (bolp))
      (insert "\n"))
  (insert (concat recipient zephyr-send-divider)))

(defun zephyr-compose (&optional recipient)
  "compose a zephyr-gram to be sent.  prompts for recipient, if none
is given, use last recipient."
  (interactive)
  (if (null recipient)
      (setq recipient
	    (completing-read "To: " 
			     (mapcar 'list (zephyr-list-previous-names)))))
  (if (not (equal mode-name "Zephyr"))
      (if zephyr-buffer-list
	  (pop-to-buffer (car zephyr-buffer-list))
	(error "no zephyr buffers")))
  (if (equal recipient "")
      (zephyr-send-setup zephyr-last-recipient)
    (zephyr-send-setup recipient)))


;;; valid values for tzcspew:
;;;   start - tzc starting up
;;;   message - an incoming message
;;;   error - an error from tzc
;;;   sent - message successfully sent
;;;   cutoff - tzc believes the server is no longer talking to it
;;;   ayt-response - tzc responding to an 'ayt' request
;;;   register-query-response - tzc responding to register-query request

;;; add most of the tags to the message alist.  since tzc prints out
;;; messages in lisp-readable from, this is mostly just a call to
;;; read.  if you were using zwgc instead, you would have to do
;;; something horrible.
;;;
;;; the tags in the alist contain at least:
;;;
;;;   message - a list of strings, one per field
;;;   instance - a string, often "PERSONAL"
;;;   auth - a symbol, either 'yes, 'no, or 'failed
;;;   kind - a symbol, usually 'acked
;;;   port - an integer, the sender's port
;;;   class - a symbol, usually 'MESSAGE
;;;   opcode - a symbol, usually either nil or 'PING
;;;   sender - a string, the login name of the sender
;;;   recipient - a string, either your login name or NIL
;;;   fromhost - a string, the hostname where the msg orginated
;;;   time - a string, just like from current-time-string
;;;
;;; these tags will sometimes be added, depending on what happens to the
;;; msg and what its contents are:
;;;
;;;   buffers - a list of buffers, where the message appeared
;;;   print-as - a string, how we should display it
;;;   signature - a string, from the sender (class MESSAGE)
;;;   body - a string, the message (class MESSAGE)
;;;   host - a string, (class LOGIN)
;;;   when - a string, (class LOGIN)
;;;   tty - a string, (class LOGIN)

(defun zephyr-parse (msg)
  (let* ((r (cdr (assq 'raw-source msg)))
	 (buf (car r))
	 (start (cadr r))
	 ;; (end (caddr r))
	 )
    (set-buffer buf)
    (goto-char start)
    (let ((newmsg (read buf)))
      ;; this makes us independent of whatever form tzc chooses to use
      ;; for opcode and class.
      (mapcar
       (if zephyr-class-and-opcode-symbols
	   (function (lambda (c) 
		       (or (null c)
			   (setcdr c (intern (zephyr-make-string (cdr c)))))))
	 (function   (lambda (c)
		       (or (null c)
			   (setcdr c         (zephyr-make-string (cdr c)) )))))
       (list (assq 'class msg)
	     (assq 'opcode msg)))
      newmsg
    )))

(defvar zephyr-tzcspew-dispatch-table
      '((start . zephyr-startup)
	(message . zephyr-pass)
	(error . zephyr-error)
	(cutoff . zephyr-cutoff)
	(sent . zephyr-sent)
	(subscribed . zephyr-subscribed)
	(not-sent . zephyr-not-sent)
	(ayt-response . zephyr-ayt-response)
	(register-query-response . zephyr-register-query-response)
	)
  "functions called by zephyr-dispatch when input arrives from tzc")

;;; dispatch the message based on the tzcspew tag
(defun zephyr-dispatch (msg)
  (let* ((spewtag (cdr (assq 'tzcspew msg))))
    (if (null spewtag)
	(error "zephyr: missing tzcspew tag - obsolete version of tzc?"))
    (let* ((spewfun (cdr (assq spewtag zephyr-tzcspew-dispatch-table))))
      (if (null spewfun)
	  (error
	   (format 
	    "zephyr: unknown tzcspew tag '%s - incompatible version of tzc?"
	    (symbol-name spewtag)))
	(funcall spewfun msg)))))

(defun zephyr-pass (msg)
  msg)

;;; Interpret the message tzc sends when it starts up
(defun zephyr-startup (msg)
  (message "zephyr: client started")
  (let ((id   (cdr (assq 'zephyrid msg))))
    (if (and id (string-match "^\\([^@]*\\)@\\(.*\\)$" id))
	(progn
	  (setq zephyr-id
		(substring id (match-beginning 1) (match-end 1)))
	  (setq zephyr-realm
		(substring id (match-beginning 2) (match-end 2))))))
  (if zephyr-extra-subscriptions
      (zephyr-subscribe zephyr-extra-subscriptions))
  nil)

(defun zephyr-subscribe (subs)
  "Request that tzc use the set of extra subscriptions SUBS."
  (zephyr-send-to-client (cons '(tzcfodder . subscribe)
			       subs)))

(defun zephyr-subscribed (msg)
  ;; new subscriptions accepted.
  nil)

(defun zephyr-error (msg)
  (let ((err (cdr (assq 'message msg))))
    (if (null err)
	(message "zephyr: tzc error")
      (message "zephyr: tzc error %s" err)))
  (beep)
  nil)

;;; tzc believes the server is no longer speaking to it.
(defun zephyr-cutoff (msg)
  (message "zephyr: tzc isn't getting messages.  Restarting it.")
  (beep)
  (zephyr-restart-receiver)
  nil)

;;; ignore ayt response.  we just send ayt to keep the zephyr-remote
;;; connection alive.
(defun zephyr-ayt-response (msg)
  nil)

;;; ignore for now -- XXX should do something if it reports error
(defun zephyr-register-query-response (msg)
  nil)

(defun zephyr-sent (msg)
  (let ((to (cdr (assq 'to msg))))
    (message "Sending to %s... done" (zephyr-make-text-recipient to)))
  nil)

(defun zephyr-not-sent (msg)
  (let ((to (cdr (assq 'to msg))))
    (message "zephyr: send to %s failed"
	     (zephyr-make-text-recipient to)))
  (beep)
  nil)

(defvar zephyr-notify-with-message-p t
  "*If t, then any message inserted in a zephyr buffer will generate
a minibuffer message.  If nil, no message will be generated.  If this
variable is set to a function name, that function will be called with the
message as its argument, and the returned value will be used to
determine whether a minibuffer message will be generated.")

(defvar zephyr-insert-p 'zephyr-default-insert-p
  "*predicate that returns true if its argument, a msg-alist, should
appear in the current buffer")

(defvar zephyr-quote-string "> "
  "String used by `zephyr-quote-message' as a quoting prefix.")

(defun zephyr-insert (msg)
  "For each zephyr buffer, check if this msg should appear in it.  If
so, run the zephyr-insert-hook-list functions for that buffer."
  (let ((buffers nil)
	(loop zephyr-buffer-list))
    (while loop
      (if (buffer-name (car loop))
	  (progn 
	    (set-buffer (car loop))
	    (if (funcall zephyr-insert-p msg)
		;; tried using save-restriction here, but it doesn't
		;; use markers, so the narrowed region grows when we
		;; insert a zgram outside it.  comment in src/editfns.c
		;; suggests that markers aren't the perfect solution,
		;; but they're better than save-restriction in this case.
		(let ((orig-restriction (if (> (buffer-size)
					       (- (point-max)(point-min)))
					    (cons (copy-marker (point-min))
						  (copy-marker (point-max)))
					  nil)))
		  (unwind-protect
		      (progn
			(buffer-flush-undo (car loop))
			(widen)
			(zephyr-do-insert-hooks msg)
			(setq buffers (cons (car loop) buffers)))
		    (buffer-enable-undo (car loop))
		    (if orig-restriction
			(narrow-to-region
			 (car orig-restriction) (cdr orig-restriction)))
		    )))))
      (setq loop (cdr loop)))
    (cons (cons 'buffers buffers) msg)))


(defun zephyr-default-insert-p (msg)
  "Determines whether MSG should be ignored or accepted, using
the zephyr-filters list.  Returns t to accept, nil to ignore."
;  (require 'timezone)
  (let ((instance   (cdr (assq 'instance msg)))
	(class      (upcase (zephyr-make-string (cdr (assq 'class msg)))))
	(opcode     (upcase (zephyr-make-string (cdr (assq 'opcode msg)))))
	(sender     (cdr (assq 'sender msg)))
	(recipient  (cdr (assq 'recipient msg)))
	(body       (cdr (assq 'body msg)))
	(signature  (cdr (assq 'signature msg)))
	(time-secs  (cdr (assq 'time-secs msg)))
	(queries    (cdr (assq 'queries msg)))
	(for-buffer (cdr (assq 'for-buffer msg)))
	;; (now        (timezone-make-date-sortable (current-time-string)))
	(l          zephyr-filters)
	(ret        t))  ; accept by default
    ;; bail if for-buffer is set and it's not this buffer.
    (if (and for-buffer (not (string-equal for-buffer (buffer-name))))
	(setq ret nil
	      l nil))
    (while l
      (let*  ((filter  (car l)))
	(while filter
	  (if (not (consp (car filter)))
	      (progn
		;; check timeout (if present) and answer
		(if (or (null (cdr filter))       ; no timeout
			(and time-secs
			     (zephyr-time-lessp time-secs (car (cdr filter)))))
		    (let ((disposition  (if (or (eq t (car filter))
						(eq nil (car filter)))
					    (car filter)
					  (funcall (car filter) msg))))
		      (if (not (eq disposition 'pass))
			  (progn
			    (setq ret disposition)
			    (setq l nil)))))   ; we have an answer
		(setq filter nil))
	    ;; looking at a (field . regexp) cons
	    (let* ((field   (car (car filter)))
		   ;; (regexp  (concat "^" (cdr (car filter)) "$"))
		   (regexp  (cdr (car filter)))
		   (field-contents  (cond ((eq 'instance field) instance)
					  ((eq 'class field) class)
					  ((eq 'opcode field) opcode)
					  ((eq 'sender field) sender)
					  ((eq 'recipient field) recipient)
					  ((eq 'body field) body)
					  ((eq 'signature field) signature)
					  ((eq 'query field)  queries)
					  (t (error
					      "bad zephyr-filter field: %s"
					      field)))))
	      (if (cond ((stringp regexp)
			 (and field-contents
			      (string-match regexp field-contents)))
			((listp field-contents)
			 (cdr (assq regexp field-contents)))
			(t
			 (eq regexp field-contents)))
		  (setq filter (cdr filter))  ; match--check rest of the filter
		(setq filter nil))))))        ; no match, try the next filter
      (setq l (cdr l)))
    ret))

;;; convert a symbol, string, number, or nil to a string
(defun zephyr-make-string (s)
  (cond
   ((null s)
    "")
   ((symbolp s)
    (symbol-name s))
   ((stringp s)
    s)
   ((numberp s)
    (number-to-string s))
   ))

(defun zephyr-warn-obsolete-var (old new)
  (if (boundp old)
      (save-excursion
 	(set-buffer (zephyr-lecture-buffer-create))
	(insert (format "Warning: you have set the obsolete variable \"%s\"."
			(symbol-name old)))
	(if new
	    (insert (format " Use %s instead." (symbol-name new))))
	(insert "\n"))))
	
;;; call each of the zephyr-insert-hook-list functions in sequence.
;;; each one must return the (possibly modified) message.  If any 
;;; one returns nil, the rest are not called.  This function is
;;; called once for each buffer in which the message should be inserted
;;; (the buffer is current).  Remember that any changes to the message
;;; will be remembered for the rest of the zephyr-insert-hook-list 
;;; functions for the current buffer only.
(defun zephyr-do-insert-hooks (msg)
  (let ((loop zephyr-insert-hook-list))
    (while (and loop msg) ; bail if the message becomes nil
      (setq msg (funcall (car loop) msg))
      (setq loop (cdr loop)))))

;;; really stick it in the current buffer.  guarantee newline termination
(defun zephyr-insert-message (msg)
  (let ((msg-banner (cdr (assq 'banner msg)))
	(msg-text   (cdr (assq 'print-as msg)))
	(insert-at  (cdr (assq 'insert-at msg))))
    (zephyr-limit-buffer-size zephyr-buffer-limit)
    (save-excursion
      (if (eq insert-at 'top)
	  (goto-char (point-min))
	(goto-char (point-max))
	(re-search-backward zephyr-send-divider-regexp (point-min) t)
	(re-search-backward "^"))
      (let ((msg-begin (point-marker)))
	(insert msg-banner)
	(let ((body-begin (point-marker)))
	  (insert msg-text)
	  (let ((len (length msg-text)))
	    (if (or (= 0 len) (not (eq ?\n (aref msg-text (- len 1)))))
		(insert "\n")))
	  ;; Tack on some buffer locations to the message.
	  ;; These should really be saved as markers, but I believe markers
	  ;; aren't gc'd (sigh), so they'll continue to slow down editing.
	  ;; Any function which modifies the message in the buffer
	  ;; should update these positions.
	  ;; 10/95 correction: I think markers *are* gc'd so maybe these
	  ;; should be changed to markers.
	  ;; 6/98 yeah, they are gc'd.  don't know what I was thinking.
	  ;;      they're markers now.
	  (append (list (cons 'msg-begin msg-begin)  ;beginning of the banner
			(cons 'msg-end (point-marker))
			(cons 'body-begin body-begin) ; beginning of msg txt
			(cons 'body-end (point-marker)))
		  msg))))))

(defun zephyr-make-message-extents (msg)
  (let ((msg-begin (cdr (assq 'msg-begin msg)))
	(msg-end (cdr (assq 'msg-end msg))))
    (if (and zephyr-receive-face zephyr-running-xemacs)
	(let ((msg-extent (make-extent msg-begin msg-end)))
	  ;; (set-extent-attribute msg-extent 'write-protected)
	  ;; skip this for the moment, I think it's a memory pig:
	  ;;(set-extent-data msg-extent (cons 'msg-data msg))
	  (set-extent-face msg-extent zephyr-receive-face)
;;;	  (if (fboundp 'set-extent-property)  ; xemacs 19.9+
;;;	      (progn (set-extent-property msg-extent 'start-open nil)
;;;		     (set-extent-property msg-extent 'end-open nil)))
)))
  msg)

;;; add the string used to "introduce" a message.
(defun zephyr-add-banner (msg)
  (let ((auth (cdr (assq 'auth msg))))
    (cons (cons 'banner
		(concat (zephyr-fromline msg)
			(cond ((eq auth 'yes) "")
			      ((eq auth 'failed) zephyr-failed-string)
			      ((eq auth 'no) zephyr-unauth-string))
			zephyr-receive-divider))
	  msg)))

;;; an example function you can add to the pipeline that
;;; timestamps messages.
(defun zephyr-add-timestamp (msg)
  (let ((banner (assq 'banner msg))
	(timestmp (cdr (assq 'time msg))))
    (if banner
	(setcdr banner (concat (cdr banner)
			       "("
			       (substring timestmp 11 16)
			       ") "))))
  msg)

;;; this one comes in handy too...
(defun zephyr-dump (msg)
  (print msg (get-buffer "*scratch*"))
  msg)

(defun zephyr-decode-fields (msg)
  (cond ((stringp msg)
	 (zephyr-decode-coding-string msg 'ctext))
	((consp msg)
	 (cons (zephyr-decode-fields (car msg))
	       (zephyr-decode-fields (cdr msg))))
	(t msg)))

; give names to the various fields and add them to the alist.  also
; add the print-as tag, containing the printed rep.
; we convert the class to uppercase, since case is supposed to be
; insignificant in class names.
(defun zephyr-parse-fields (msg)
  (let ((class (intern (upcase (zephyr-make-string (cdr (assq 'class msg))))))
	(urgent (string-equal (downcase (cdr (assq 'instance msg))) "urgent"))
	(sender (cdr (assq 'sender msg)))
	(opcode (intern (upcase (zephyr-make-string
				 (cdr (assq 'opcode msg))))))
	(fields (cdr (assq 'message msg)))
	(time-secs  (cdr (assq 'time-secs msg))))
    ;; this is for a kludge to let emacs18 know (sort of) what time it is
    (if time-secs
	(setq zephyr-last-zgram-timestamp time-secs))
    (cond (; in logins, the fields are host, when, and tty.
	   (eq class 'LOGIN)
	   (let ((host (nth 0 fields))
		 (when (nth 1 fields))
		 (tty (nth 2 fields))
		 (type (cond ((eq opcode 'USER_LOGIN) "login")
			     ((eq opcode 'USER_LOGOUT) "logout")
			     (t "<weird LOGIN opcode>"))))
	     (append (list (cons 'print-as
				 (concat type " on " host " at " when))
			   (cons 'host host)
			   (cons 'when when)
			   (cons 'tty tty))
		     msg)))
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
			      ((= 1 len) "")   ; usually just means empty body
			      ;; zwgc just ignores extra fields, so we will too
			      ;; ((not (= len 2)) (format "malformed message: %s" fields))
			      (t (cadr fields))
			      )))
	     (append (list (cons 'print-as body)
			   (cons 'signature sig)
			   (cons 'body body))
		     msg)))
;;	  (t (cons (cons 'print-as
;;			 (mapconcat '(lambda (x) x)
;;				    fields "\n"))
;;		   msg))
	  )))

(defun zephyr-personal-msg-p (msg)
  (let ((recip  (cdr (assq 'recipient msg))))
    (and (stringp recip)
	 (not (string-equal recip ""))
	 (not (eq ?@ (aref recip 0))))))

(defvar zephyr-auto-reply-msg "Sorry, I can't answer your zephyrgram now.

This is a recording."
  "Reply sent if zephyr-auto-reply is in the zephyr-hook-list.")

(defvar zephyr-auto-reply-users nil
  "alist of (recipient . (current-time)) indicating the last time
each user got an autoreply.")

(defvar zephyr-auto-reply-timeout 60
  "don't reply to the same user twice within this many seconds")

(defun zephyr-auto-reply (msg)
  "Send the message in zephyr-auto-reply-msg to anyone who sends 
you a personal zgram.  Won't send to the same sender twice in 60 
seconds (zephyr-auto-reply-timeout), to prevent loops.

You could put this function in zephyr-hook-list, just before zephyr-insert,
to auto-reply to zephyrs.  Modify to taste.

Doesn't work in emacs18."
  (let* ((sender    (cdr (assq 'sender msg)))
	 (opcode    (zephyr-make-string (cdr (assq 'opcode msg))))
	 (last-send (assoc sender zephyr-auto-reply-users))
	 (now       (zephyr-current-time)))
    (if (and (or (null last-send)
		 (> (zephyr-time-diff (cdr last-send) now) 
		    zephyr-auto-reply-timeout))
	     (zephyr-personal-msg-p msg)
	     (string-equal "" opcode))
	(progn
	  (funcall zephyr-send-function zephyr-auto-reply-msg
		   (list (list "MESSAGE" "PERSONAL" sender)))
	  (if last-send
	      (setcdr last-send now)
	    (setq zephyr-auto-reply-users (cons (cons sender now) 
						zephyr-auto-reply-users))))))
  msg)

(defun zephyr-lazy-beep (now delay)
  (let ((then zephyr-lazy-beep-last))
    (setq zephyr-lazy-beep-last now) ; horrid global var
    (if (and delay (or (not then)
		       (> (zephyr-time-difference-in-seconds then now)
			  delay)))
	(beep))))

;;;
;;;; convert binary to ascii, slow stupid, simple.
;;;(defun zephyr-btoa (s)
;;;  (mapconcat '(lambda (c) (int-to-string c)) s " "))
;;;
;;;; convert ascii to binary, slow stupid, simple.
;;;; returns nil if there is a formatting error
;;;(defun zephyr-atob (s)
;;;  (save-excursion
;;;    (let ((src (generate-new-buffer " atob-src"))
;;;	  ans)
;;;      (set-buffer src)
;;;      (insert "(" s ")")
;;;      (goto-char (point-min))
;;;      (setq ans
;;;	    (condition-case ERR
;;;		(mapconcat 'char-to-string (read src) "")
;;;	      (error nil)))
;;;      (kill-buffer src)
;;;      ans)))
;;;
;;;; en/decrype S using KEY.  EN is true means encrypt, otherwise
;;;; decrypt.
;;;(defun zephyr-en/decrypt (s key en)
;;;  (save-excursion
;;;    (let ((in (generate-new-buffer " endecrypt-in"))
;;;	  (out (generate-new-buffer " endecrypt-out"))
;;;	  (pgm (if en zephyr-encrypt-program
;;;		 zephyr-decrypt-program)))
;;;      (set-buffer in)
;;;      (insert s)
;;;      (call-process-region (point-min) (point-max)
;;;			   pgm t out nil key)
;;;      (kill-buffer in)
;;;      (set-buffer out)
;;;      (let ((b (buffer-string)))
;;;	(kill-buffer out)
;;;	b))))


(defun zephyr-notify-with-message-p (msg)
  "Using the value of the zephyr-notify-with-message-p variable,
determine whether this message warrants a minibuffer notification."
  (if (fboundp zephyr-notify-with-message-p)   ; the variable!
      (funcall zephyr-notify-with-message-p msg)
    zephyr-notify-with-message-p))

(defun zephyr-notify-with-message (msg)
  (if (zephyr-notify-with-message-p msg)
      (let* ((instance     (cdr (assq 'instance msg)))
	     (class        (zephyr-make-string (cdr (assq 'class msg))))
	     (lc-instance  (cond
			    ((null instance)                    "instanceless")
			    ((string-equal instance "PERSONAL") "personal")
			    (t                                  instance))))
	(message "received %s zephyrgram from %s"
		 (zephyr-class-instance-string class lc-instance)
		 (or (cdr (assq 'signature msg))
		     (cdr (assq 'sender msg))
		     "???"))))
  msg)

(defun zephyr-notify-with-beep (msg)
  (let ((instance (cdr (assq 'instance msg)))
	(time (cdr (assq 'time msg))))
    (if (not (equal (downcase instance) "urgent"))
	(if (or (null zephyr-beep-personal-only)
		(zephyr-personal-msg-p msg))
	    (zephyr-lazy-beep time zephyr-lazy-beep-time))
      (beep) (beep) (beep)))
  msg)

;;; if the window where the messages appears is active, move so the
;;; end (where the message is) is visible
;;; UNUSED??
;;;(defun zephyr-buffer-show-end (buf)
;;;  (let* ((win (get-buffer-window buf)))
;;;    (if win
;;;	(progn
;;;	  (set-buffer buf)
;;;	  (if (pos-visible-in-window-p (point-max) win)
;;;	      nil
;;;	    (goto-char (point-max))
;;;	    (vertical-motion (- 2 (window-height win)))
;;;	    (set-window-start win (point)))))))


(defun zephyr-notify-with-scroll (msg)
  "This function is the source of endless trouble.  Avoid if at all possible."
  (let ((cursor-win (selected-window)))
    (let* ((buf (current-buffer))
	   (win (if zephyr-running-xemacs
		    (get-buffer-window buf t t)  ; xemacs
		  (if (or (and (boundp 'epoch::version) epoch::version)
			  (not zephyr-running-recent-fsf))
		      (get-buffer-window buf)    ; fsf v18 or epoch
		    (get-buffer-window buf t)))))  ; fsf v19
      (if (and win
	       ;; don't scroll the selected window
	       (not (eq win cursor-win))
	       ;; don't scroll if there's a send-divider below point
	       (not (save-excursion
		      (re-search-forward  (concat "^.+\\("
						  zephyr-send-divider-regexp
						  "\\)")
					  nil t))))
	  (progn
	    (goto-char (point-max))
	    ;; FSF added the win argument sometime between 19.22 and 19.25
	    (if (or zephyr-running-xemacs
		    (and (boundp 'emacs-major-version)
			 (= emacs-major-version 19)
			 (>= emacs-minor-version 25)))
		(vertical-motion (- 2 (window-height win)) win)
	      (vertical-motion (- 2 (window-height win))))
	    (set-window-start win (point))))))
  msg)

(defun zephyr-fromline (msg)
  "return the from-line for MSG.  e.g. \"susan(graffiti)\""
  (let* ((inst  (cdr (assq 'instance msg)))
	 (class (zephyr-make-string (cdr (assq 'class msg))))
	 (recip (zephyr-alias-realm (cdr (assq 'recipient msg))))
	 (sender (zephyr-alias-realm (cdr (assq 'sender msg))))
	 (inst-realm (if (and (> (length recip) 0)
			      (eq (aref recip 0) ?@))
			 (concat inst recip)
		       inst)))
    (if (and (string-equal (upcase inst) "PERSONAL")
	     (string-equal (upcase class) "MESSAGE")
	     (zephyr-personal-msg-p msg))
	sender
      (concat sender "("
	      (zephyr-class-instance-string class inst-realm)
	      ")"))))

; PINGs just generate a message in the minibuffer
(defun zephyr-ping-notify (msg)
  (if (string-equal "PING" (zephyr-make-string (cdr (assq 'opcode msg))))
      (progn
	(if (zephyr-notify-with-message-p msg)
	    (message "zephyr PING from %s" (zephyr-fromline msg)))
	nil)  ; return nil so that nothing else happens w/ this message.
    msg))     ; not a PING, so pass it on.


(defun zephyr-notify-with-deiconify (msg)
  (if (and (or (fboundp 'buffer-dedicated-frame) 
	       (fboundp 'buffer-dedicated-screen))
	   zephyr-deiconify-on-receive)
      (let ((buf (current-buffer)))
	(let ((s (cond ((fboundp 'buffer-dedicated-frame)
			(buffer-dedicated-frame buf))
		       ((fboundp 'buffer-dedicated-screen)
			(buffer-dedicated-screen buf)))))
	  (if s
	      (cond ((fboundp 'deiconify-frame)
		     (deiconify-frame s))
		    ((fboundp 'deiconify-screen)
		     (deiconify-screen s))
		    (t
		     (error "zephyr-notify-with-deiconify: no deiconify fn"))))
	  )))
  msg)

;; If current buffer has grown larger than maxsize, trim it at the 
;; top to ~90% maxsize, but never delete beyond delete-limit.
(defun zephyr-limit-buffer-size (maxsize &optional delete-limit)
  (if maxsize
      (if (> (- (point-max) (point-min)) maxsize)
	  (let ((delete-to  (- (point-max) (* (/ maxsize 100) 90))))
	    (delete-region (point-min) 
			   (max (point-min)
				(if delete-limit
				    (min delete-to delete-limit)
				  delete-to)))))))

(defun zephyr-receive-sentinel (proc sig)
  (if (memq (process-status proc) '(exit closed signal))
      (progn
        (message "zephyr-receive died")        
	(pop-to-buffer (process-buffer proc)))))

(defun zephyr-do-message-hooks (msg)
  (if (memq 'zephyr-notify-with-message zephyr-hook-list)
      (progn
	(message "Warning: found old style zephyr-hook-list.  Check documentation.")
	(sleep-for 1)))
  (let ((loop zephyr-hook-list))
    (while (and loop msg) ; bail if the message becomes nil
      (setq msg (funcall (car loop) msg))
      (setq loop (cdr loop)))))

(defun zephyr-receive-filter (process string)
  (save-excursion
    (let ((log-buf (process-buffer process)))
      (set-buffer log-buf)
      (let ((start (point-max)))
	(goto-char start)
	(insert string)
	(goto-char start)
	(while (search-forward zephyr-client-eom-string
			       (point-max) t)
	  (let ((end (point)))
	    (forward-char -1)
	    (if (search-backward zephyr-client-bom-string
				 (point-min) t)
		(forward-char 1)
	      (goto-char (point-min)))
	    (zephyr-do-message-hooks
	     (list (list 'raw-source
			 log-buf (point) end)))
	    (set-buffer log-buf)
	    (goto-char end)
	    (zephyr-limit-buffer-size zephyr-log-buffer-limit end)))))))

(defun zephyr-restarting-receive-sentinel (proc &optional sig)
  (if (not (memq (process-status proc) '(run open)))
      (progn
	(set-process-sentinel proc nil)
	(delete-process proc)
	(zephyr-start-receiver)
;       (message "Restarting zephyr receiver process...done")
	)))

(defun zephyr-restart-receiver ()
  "kill and start another receiver process.  this is a good thing to do if
your kerberos tickets expire, causing all messages authentication to
appear failed."
  (interactive)
  ;; (message "Restarting zephyr receiver process...")
  (set-process-sentinel zephyr-process 'zephyr-restarting-receive-sentinel)
  (if (memq (process-status zephyr-process) '(run open))
      (interrupt-process zephyr-process)   ; send SIGINT
    (zephyr-restarting-receive-sentinel zephyr-process)))

(defun zephyr-start-receiver ()
  (setq zephyr-lazy-beep-last nil)
  (setq zephyr-process
	(let ((process-connection-type zephyr-process-connection-type))
	  (apply 'start-process
		 "zephyr-receive" zephyr-log-buffer
		 zephyr-receive-program)))
  (and (fboundp 'set-buffer-process-coding-system)
       (save-excursion
	 (set-buffer zephyr-log-buffer)
	 (set-buffer-process-coding-system 'binary 'binary)))
  (fsf/x (set-process-query-on-exit-flag zephyr-process nil)
         (process-kill-without-query zephyr-process))
  (set-process-sentinel zephyr-process 'zephyr-receive-sentinel)
  (set-process-filter zephyr-process 'zephyr-receive-filter))

(defvar zephyr-previous-names nil
  "doubly linked list of names of destinations and sources of
zephyrgrams previously sent and received.  most recent is first.  no
duplicates.")

(or (fboundp 'cadr)
    (defun cadr (l) (car (cdr l))))
(or (fboundp 'cddr)
    (defun cddr (l) (cdr (cdr l))))
(or (fboundp 'cdddr)
    (defun cdddr (l) (cdr (cdr (cdr l)))))
(or (fboundp 'cdadr)
    (defun cdadr (l) (cdr (car (cdr l)))))
(or (fboundp 'caddr)
    (defun caddr (l) (car (cdr (cdr l)))))
(or (fboundp 'buffer-flush-undo)
    (defalias 'buffer-flush-undo 'buffer-disable-undo))

(defun zephyr-touch (msg)
  "touch the name(s) appearing in msg"
  (let* ((sender    (zephyr-alias-realm (cdr (assq 'sender msg))))
	(instance  (zephyr-alias-realm (cdr (assq 'instance msg))))
	(class     (zephyr-make-string (cdr (assq 'class msg))))
	(body      (cdr (assq 'body msg)))
	(recipient (cdr (assq 'recipient msg)))
	(inst-realm (if (and (> (length recipient) 0)
			     (eq (aref recipient 0) ?@))
			(concat instance recipient)
		      instance))
	(classinst (zephyr-class-instance-string class inst-realm))
	(case-fold-search nil))
    (if (zephyr-personal-msg-p msg)
	(if (string-equal "PERSONAL" instance)
	    (zephyr-touch-name sender)
	  (zephyr-touch-name (concat sender "(" classinst ")")))
      (zephyr-touch-name sender)
      (zephyr-touch-name (concat "(" classinst ")")))
    ;; if the first line of the message is a list of recipients
    ;; like "[larry curly moe]", the sender is "shemp", and the
    ;; recipient (this user) is "curly", then touch the list
    ;; of names "shemp,larry,moe".  (see zephyr-send-show-multiple)
    (if (and body
	     (equal 0 (string-match "[ \t]*\\[" body)))
	(let ((start-pos  (match-end 0))
	      (names      sender)
	      (num        0))
	  (while (and (>= num 0)
		      (equal start-pos (string-match "[ \t\n]*\\([^] \t\n]+\\)"
						     body start-pos)))
	    (let ((userid  (substring body (match-beginning 1) (match-end 1))))
	      (if (not (string-equal userid recipient))
		  (progn (setq names (concat names "," userid))
			 (setq num (+ 1 num))))
	      (setq start-pos (match-end 0))
	      (if (not (string-match "^(?[a-z0-9.-]+\\(@[a-zA-Z0-9.-]+\\|\\))?$"
				     userid))
		  ;; uh oh, this isn't really a to-list
		  (setq num -1))))
	  (if (and (> num 0)
		   (equal start-pos
			  (string-match "[ \t\n]*\\][ \t]*$" 
					body start-pos)))  ; closing `]'
	      (zephyr-touch-name names)))))
  msg)

(defvar zephyr-history-ptr nil)

(defun zephyr-touch-name (name)
  "move NAME to head zephyr-previous-names, add if not already there.
If the name matches zephyr-history-ignore-regexp, then don't add it."
  (if (not (string-match zephyr-history-ignore-regexp name))
      (progn
	(if zephyr-previous-names
	    (if (not (equal name (car zephyr-previous-names)))
		(progn
		  (let ((h (cddr zephyr-previous-names)))
		    (while (not (eq h zephyr-previous-names))
		      (if (equal (car h) name)
			  (progn
			    (setcar (cdddr h) (cadr h))
			    (setcdr (cdadr h) (cddr h))
			    (setq h zephyr-previous-names))
			(setq h (cddr h)))))
		  (let ((n (cons name (cons (cadr zephyr-previous-names)
					    zephyr-previous-names))))
		    (setcar (cdddr n) n)
		    (setcdr (cdadr n) n)
		    (setq zephyr-previous-names n))))
	  (let ((n (cons name (cons nil nil))))
	    (setcar (cdr n) n)
	    (setcdr (cdr n) n)
	    (setq zephyr-previous-names n)))
	(setq zephyr-history-ptr (cadr zephyr-previous-names))))
  nil)      ; return nil so we can print result w/o looping when debugging

(defun zephyr-list-previous-names (&optional n)
  "return a list of previous zephyr destinations.  Optional argument
N means only return the N most recent destinations.  Most recent is
listed first."
  (if (and zephyr-previous-names
	   (or (null n) (> n 0)))
      (let ((l (list (car zephyr-previous-names)))
	    (h (cddr zephyr-previous-names))
	    (nn 1))
	(while (and h
		    (not (eq h zephyr-previous-names))
		    (or (null n) (< nn n)))
	  (setq l (cons (car h) l))
	  (setq h (cddr h))
	  (setq nn (+ nn 1)))
	(nreverse l))
    nil))

(defvar zephyr-menu
  '(["Compose Zgram..." zephyr-compose t]
    ["Send Zgram" zephyr-send-and-compose t]
    ("Subscriptions" 
     ["Ignore instance temporarily..." zephyr-ignore-instance-temporarily t]
     ["Listen to instance temporarily..." zephyr-listen-instance-temporarily t]
     ["Ignore sender temporarily..." zephyr-ignore-sender-temporarily t]
     ["Listen to sender temporarily..." zephyr-listen-sender-temporarily t]
     ["Listen to class..." zephyr-add-class-and-restart t])
    ["Restart Receiver" zephyr-restart-receiver t])
  "Menu for zephyr mode.")

(defvar zephyr-menu-keymap 
  (and zephyr-menu zephyr-running-fsf19
       (easy-menu-create-keymaps "Zephyr" zephyr-menu))
  "Holds the zephyr menu keymap for FSF GNU Emacs.")

(defvar zephyr-history-menu-names 10
  "Number of names on the \"Previous Destinations\" zephyr menu.  
If nil, list all names.")

(defun zephyr-history-menu ()
  (if (eq 0 zephyr-history-menu-names)
      nil
    (let ((names  (zephyr-list-previous-names zephyr-history-menu-names)))
      (mapcar (function (lambda (n)
			  (vector n (list 'zephyr-compose n) t)))
	      names))))

(defun zephyr-update-menu ()
  "for use on the activate-menubar-hook"
  (if (eq major-mode 'zephyr-mode)
      (let ((recips  (zephyr-history-menu)))
      (if recips
          (cond (zephyr-running-xemacs
                 (add-menu '("Zephyr") "Recipients" recips))
                (zephyr-running-fsf19
                 (define-key zephyr-mode-map [menu-bar Zephyr Recipients]
                   (cons "Recipients" (easy-menu-create-keymaps 
                                       "Recipients" recips))))
                (zephyr-running-fsf20
                 (easy-menu-define zephyr-recipients-menu zephyr-menu-keymap
                                   "Zephyr Recipients" (cons "Recipients"
                                                             recips))))
        nil)) ;; says changes may have been made
    t)) ;; says no changes have been made

(defun zephyr-popup-menu ()
  "pop up the zephyr menu"
  (interactive)
  (if zephyr-running-xemacs
      (let* ((recips   (zephyr-history-menu))
           (menu     (if recips
                         (append zephyr-menu (list (cons "Recipients"
                                                         recips)))
                       zephyr-menu)))
      ;; if both zephyr-menu and (zephyr-history-menu) return nil, no menu.
      (if menu
          (popup-menu (cons "Zephyr Commands" menu))))
    (let ((menu (key-binding [menu-bar Zephyr])))
      (if menu
        (x-popup-menu t menu)))))

(defun zephyr-create-menu ()
  "modify this buffer's menubar to include the zephyr menu.
won't create a menubar if one doesn't already exist."
  (interactive)
  (cond ((and zephyr-running-xemacs zephyr-menu current-menubar)
       (progn
         ;; have to do the copy because the add-menu in zephyr-update-menu
         ;; does a destructive update, which would clobber zephyr-menu
         (set-buffer-menubar current-menubar)
         (add-menu nil "Zephyr" (copy-tree zephyr-menu))
         (add-hook 'activate-menubar-hook 'zephyr-update-menu)))
      ((and zephyr-running-fsf19 zephyr-menu)
       (progn
         (define-key zephyr-mode-map [menu-bar Zephyr]
           (cons "Zephyr" (copy-keymap zephyr-menu-keymap)))
         (add-hook 'menu-bar-update-hook 'zephyr-update-menu)))
      ((and zephyr-running-fsf20 zephyr-menu)
       (progn
         (easy-menu-define zephyr-menu-keymap zephyr-mode-map
                           "Zephyr Menu" (cons "Zephyr" zephyr-menu))
         (add-hook 'menu-bar-update-hook 'zephyr-update-menu)))))

(defun zephyr-replace-destination (name)
  "replace the current destination with NAME.
returns nil if name is already the current destination, t otherwise."
  (save-excursion
   (re-search-backward zephyr-send-divider-regexp)
   (let ((end-dest (point)))
     (re-search-backward "^")
     (let ((current  (zephyr-buffer-substring-no-properties (point) end-dest)))
       (if (string-equal current name)
	   nil
	 (delete-region (point) end-dest)
	 (insert name)
	 t)))))

(defun zephyr-next-destination (arg)
  "cycle forward through previous senders/destinations.
If the previous destination is the same as the current destination, 
then get the one after that."
  (interactive "*p")
  (if zephyr-history-ptr
      (if (not (and (<= arg 0)
		    (or (zephyr-replace-destination (car zephyr-history-ptr))
			(< arg 0))))
	  (progn
	    (setq zephyr-history-ptr (cadr zephyr-history-ptr))
	    (zephyr-next-destination (- arg 1))))))

(defun zephyr-previous-destination (arg)
  "cycle backward through previous senders/destinations.
If the next destination is the same as the current destination, 
then get the one after that."
  (interactive "*p")
  (if zephyr-history-ptr
      (if (not (and (<= arg 0)
		    (or (zephyr-replace-destination (car zephyr-history-ptr))
			(< arg 0))))
	  (progn
	    (setq zephyr-history-ptr (cddr zephyr-history-ptr))
	    (zephyr-previous-destination (- arg 1))))))

(defun zephyr-goto-beginning-of-input ()
  "Skip to the beginning of the message being composed."
  (interactive)
  (goto-char (zephyr-beginning-of-input)))

(defun zephyr-kill-input ()
  "Kill the text of the message being composed."
  (interactive)
  (let ((start-msg (zephyr-beginning-of-input)))
    (kill-region start-msg (point))))

(defun zephyr-beginning-of-input ()
  (save-excursion 
    (let ((pat (concat "^.+\\("
			      zephyr-send-divider-regexp
			      "\\)")))
      (progn (re-search-backward pat)
	     (match-end 0)))))

(defun zephyr-delete-messages-from (inst)
  "delete all messages on a particular instance or sender that appear after
point.  takes a regexp.  (The function's name is somewhat deceptive.)"
  (interactive "sInstance/Sender name (regexp): ")
  (let* ((receive-divider-regexp (regexp-quote zephyr-receive-divider))
	 (kill (concat "\\("
		       inst		; takes a regexp
		       "\\).*"
		       receive-divider-regexp))
	 (any-divider-regexp (concat "\\("
				     receive-divider-regexp
				     "\\|"
				     zephyr-send-divider-regexp
				     "\\)")))
    (while (and (not (eobp))
		(re-search-forward kill nil t))
      (beginning-of-line 1)
      (let ((p (point))
	    (found (re-search-forward any-divider-regexp nil t 2)))
	(beginning-of-line 1)
	(if found 
	    (delete-region p (point))
	  (end-of-line 1))))))

;;; (defun zephyr-vector-to-time-string (time hrs mins)
;;;   (format "%d %s %d %02d:%02d:%02d"
;;; 	  (elt time 2)
;;; 	  (car (nth (- (elt time 1) 1) timezone-months-assoc))
;;; 	  (elt time 0)
;;; 	  (+ hrs (elt time 3))
;;; 	  (+ mins (elt time 4))
;;; 	  (elt time 5)))
;;;
;;; (defun zephyr-future-date (n)
;;;   "Produce a date string whihch is n minutes in the future.  24 hours max.
;;; No guarantees around daylight savings time boundaries, etc."
;;;   (if (> n (* 24 60))
;;;       (error "zephyr-future-date: more than 24 hours in the future"))
;;;   (let ((hrs   (/ n 60))
;;; 	(mins  (mod n 60))
;;; 	(v1    (timezone-fix-time (current-time-string) "GMT" "GMT")))
;;;     (zephyr-vector-to-time-string
;;;      (timezone-fix-time (zephyr-vector-to-time-string v1 hrs mins)
;;; 			"GMT" "GMT")
;;;      0 0)))

(defun zephyr::temp-subscr (regexp mins accept-flag field)
  (if (not (eq major-mode 'zephyr-mode))
      (error "error: you're not in a zephyr buffer"))
  (let* ((int-mins    (cond ((stringp mins) 
			     (if (string= mins "") 
				 nil
			       (string-to-int mins)))
			    ((integerp mins) mins)
			    (t mins)))
	 (filter      (append (list (cons field regexp))
			      (list (cons 'recipient "^\\(@\\|$\\)"))
			      (if accept-flag
				  (list (cons 'opcode "^$"))
				nil)
			      (list accept-flag)))
	 (timeout     (if int-mins
			  (list 
			   (zephyr-add-secs (zephyr-current-time)
					    (* 60 int-mins)))
			nil)))
    (setq zephyr-filters (cons (append filter timeout) zephyr-filters))))

(defun zephyr-ignore-instance-temporarily (regexp mins)
  "Ignore broadcast zephyrgrams on instances matching REGEXP for the
next MINS minutes."
  (interactive "sIgnore instance (regexp): 
sIgnore for how many minutes: [default forever] ")
  (zephyr::temp-subscr regexp mins nil 'instance))

(defun zephyr-listen-instance-temporarily (regexp mins)
  "Accept broadcast zephyrgrams on instances matching REGEXP for the
next MINS minutes."
  (interactive "sListen to instance (regexp): 
sListen for how many minutes: [default forever] ")
  (zephyr::temp-subscr regexp mins t 'instance))

(defun zephyr-ignore-sender-temporarily (regexp mins)
  "Ignore broadcast zephyrgrams from senders matching REGEXP for the
next MINS minutes."
  (interactive "sIgnore sender (regexp): 
sIgnore for how many minutes: [default forever] ")
  (zephyr::temp-subscr regexp mins nil 'sender))

(defun zephyr-listen-sender-temporarily (regexp mins)
  "Accept broadcast zephyrgrams from senders matching REGEXP for the
next MINS minutes."
  (interactive "sListen to sender (regexp): 
sListen for how many minutes: [default forever] ")
  (zephyr::temp-subscr regexp mins t 'sender))

;;; XXX should this do something with zephyr-filters too?
(defun zephyr-add-class-and-restart (class instance realm)
  "Add (CLASS INSTANCE \"*\") to zephyr-extra-subscriptions,
and restart tzc (instance \"*\" is wildcard)."
  (interactive 
   (list
     (read-from-minibuffer "Listen to which class? [MESSAGE] ")
     (read-from-minibuffer "Which instance? [*] ")
     (read-from-minibuffer 
      (format "Which realm? [%s] " zephyr-realm))))
  (let ((class    (if (string-equal class "")
		      "MESSAGE"
		    class))
	(instance (if (string-equal instance "") 
		      "*" 
		    instance))
	(realm    (if (string-equal realm "")
		      zephyr-realm
		    realm)))
    (setq zephyr-extra-subscriptions 
	  (cons (list (upcase class) instance (concat "*@" realm))
		zephyr-extra-subscriptions))
    (message "Restarting receiver...")
    (zephyr-restart-receiver)))

(defun zephyr-add-secs (tm secs)
  "add SECS seconds to the time represented by TM"
  (let ((s  (+ secs (nth 1 tm))))
    (list
     (+ (nth 0 tm) (/ s 65536))
     (mod s 65536)
     (nth 2 tm))))

(defun zephyr-time-lessp (t1 t2)
  "does time T1 precede T2?  (t1 and t2 are lists of 3 ints, as
in (current-time)"
  (or (< (nth 0 t1) (nth 0 t2))
      (and (= (nth 0 t1) (nth 0 t2))
	   (< (nth 1 t1) (nth 1 t2)))
      (and (= (nth 0 t1) (nth 0 t2))
	   (= (nth 1 t1) (nth 1 t2))
	   (< (nth 2 t1) (nth 2 t2)))))
	   
(defun zephyr-time-diff (t1 t2)
  "return (T2 - T1) as a float.  Requires a vintage-19 emacs."
  (+ 
   (* 65536 (- (nth 0 t2) (nth 0 t1)))
   (- (nth 1 t2) (nth 1 t1))
   (/ (- (nth 2 t2) (nth 2 t1)) (read "1000000.0"))))

(defun zephyr-current-time ()
  "return the current time as a list of 3 ints, using the format
of (current-time)"
  (if (fboundp 'current-time)  ; fsf19, xemacs19.9+
      (current-time)
    (if (fboundp 'current-time-seconds)  ; xemacs 19.8-
	(let ((tm (current-time-seconds)))
	  (list (car tm) (cdr tm) 0))
      (if zephyr-last-zgram-timestamp  ; desperation (emacs18)
	  zephyr-last-zgram-timestamp
	(error "don't know what time it is until we get a zgram")))))

(defun zephyr-get-message-info ()
  "Display the timestamp, sender, and host for the received message at point.
(This feature is a little buggy right now, since the extents seem to 
overlap sometimes for some reason.)"
  (interactive)
  (error "zephyr-get-message-info is (perhaps temporarily) disabled")
  (if (not zephyr-running-xemacs)
      (message "You can only use this feature with XEmacs (aka lemacs).")
    (let ((msg nil))
      ;; this will stop checking as sooon as it finds the message
      (map-extents '(lambda (e junk) (if (eq (car (extent-data e)) 'msg-data)
					 (setq msg (cdr (extent-data e)))
				       nil))
		   (current-buffer)
		   (point) (point))
      (if (null msg)
	  (message "Point is not in a received message.")
	(let* ((time       (cdr (assq 'time msg)))
	       (no-year    (substring time 0 -5))
	       ;; (instance   (cdr (assq 'instance msg)))
	       (sender     (or (cdr (assq 'sender msg)) "???"))
	       (sig        (cdr (assq 'signature msg)))
	       (fromhost   (cdr (assq 'fromhost msg))))
	  (message "%s, from %s@%s (%s)"
		   no-year sender fromhost sig))))))

(defvar zephyr-log-buffer nil)

(defun zephyr-mode ()

  "major mode for sending and receiving zephyr-grams.  use
zephyr-send-and-compose [\\[zephyr-send-and-compose]] to send messages.
instances are specified by enclosing their names in parentheses.  multiple
destinations are separated by whitespace or commas.  to change the
destination just edit it, or use zephyr-compose [\\[zephyr-compose]].  
if you want to send an instance to just one person, use \"user(instance)\".

in the composition buffer, the destinations for the current message
appear to the left of \"<<< \".  when you send the zgram, everything
between point and \"<<< \" will be transmitted.  at any time, you can
edit the current destinations, or go back to previous messages and
edit/send them.

when a message arrives, a beep will sound, unless message has arrived
in the previous 120 (the value of zephyr-lazy-beep-time, really)
seconds.  for more elaborate notification, use zephyr-notify-hook.

the output of the receiver process is kept in *log-zephyr* buffer.  the
zephyr-log-buffer-limit and zephyr-buffer-limit variables control how
much text is saved in the buffers.  additional text is discarded.

\\{zephyr-mode-map}

this mode is highly customizable, there are many hooks and variables
you can use to change how it behaves.  here's some of what you can do:

  visibly time-stamp incoming messages (see zephyr-insert-hook-list)

  filter out particular instances/users (see
     zephyr-filters)

  multiple receiving buffers with different hooks and regexps

  define aliases for sending to common groups of people (see
     zephyr-aliases)"

  (interactive)
  (kill-all-local-variables)

;;  (make-local-variable 'zephyr-opcodes-ignore-list)
;;  (make-local-variable 'zephyr-opcodes-accept-list)
;;  (make-local-variable 'zephyr-senders-ignore-regexp)
;;  (make-local-variable 'zephyr-senders-accept-regexp)
;;  (make-local-variable 'zephyr-recipients-ignore-regexp)
;;  (make-local-variable 'zephyr-recipients-accept-regexp)
;;  (make-local-variable 'zephyr-recipients-always-accept-regexp)
;;  (make-local-variable 'zephyr-instances-ignore-regexp)
;;  (make-local-variable 'zephyr-instances-accept-regexp)
  (make-local-variable 'zephyr-previous-names)
  (make-local-variable 'zephyr-history-ptr)
  (make-local-variable 'zephyr-last-recipient)
  (make-local-variable 'zephyr-filters)

  (make-local-variable 'zephyr-lecture-given)
  
  ;; hmm, should we do this?
  ;; (make-local-variable 'zephyr-history-ignore-regexp)

  (setq zephyr-lecture-given nil)

  (set-syntax-table text-mode-syntax-table)
  (use-local-map zephyr-mode-map)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq major-mode 'zephyr-mode)
  (setq mode-name "Zephyr")
  (or zephyr-id
      (setq zephyr-id (user-login-name)))
  (or zephyr-signature
      (setq zephyr-signature (user-full-name)))

  (zephyr-create-menu)

  (if (or  (not zephyr-log-buffer)
           (null (buffer-name zephyr-log-buffer)))
      (save-excursion
	(set-buffer
	 (setq zephyr-log-buffer (get-buffer-create zephyr-log-buffer-name)))
 	(and (fboundp 'set-buffer-multibyte) (set-buffer-multibyte nil))
	))
  ;; could just make the buffer name start with space and get the same effect,
  ;; but people are used to the old name.
  (if (fboundp 'buffer-disable-undo)
      (buffer-disable-undo zephyr-log-buffer))
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "\\(" paragraph-start "\\|"
				"^.*" zephyr-send-divider-regexp "\\|"
				"^.*" zephyr-receive-divider-regexp "\\)"))

  ;; start up a tzc if one doesn't already exist
  (if (not (and (boundp 'zephyr-process)
                (processp zephyr-process)
                (memq (process-status zephyr-process) '(open run))))
      (zephyr-start-receiver))

  (run-hooks 'text-mode-hook 'zephyr-mode-hook)

  (zephyr-warn-obsolete-var 'zephyr-send-hook 'zephyr-send-function)
  (zephyr-warn-obsolete-var 'zephyr-instance-xlate-hook
			    'zephyr-instance-xlate-function)

  ;; take zephyr-initial-history as the previous-recipients list,
  ;; and set zephyr-last-recipient to the last element of that list.
  ;; for backwards compatibility, don't set zephyr-last-recipient
  ;; if the user sets it explicitly.
  (if (not zephyr-last-recipient)
      (if (null zephyr-initial-history)
          (setq zephyr-last-recipient "nobody")
        (setq zephyr-last-recipient
              (elt zephyr-initial-history
                   (- (length zephyr-initial-history) 1)))))
  (let ((l  zephyr-initial-history))
    (while l
      (zephyr-touch-name (car l))
      (setq l (cdr l))))
  
  (if zephyr-lecture-given
      (let ((b (current-buffer)))
	(pop-to-buffer (get-buffer "*zephyr-lecture*"))
	(goto-char (point-min))
	(set-buffer b)))

;;; end of zephyr-mode
)

(defun zephyr-add-hook (hooklist f &optional before)
    "Add function F to the hook-list HOOKLIST, inserting just before the
item BEFORE.  If BEFORE is nil, insert at end of the list.  
Returns the new hooklist.  Example:
  (zephyr-add-hook 'zephyr-hook-list 'munge-message 'zephyr-insert))"
    (if (and before (not (symbolp before)))
	(error "`before' argument is not a symbol"))
    (let ((l (reverse (symbol-value hooklist)))
	  (ret (if before nil (list f))))
      (while l
	(setq ret (cons (car l) ret))
	(if (and before (eq (car l) before))
	    (setq ret (cons f ret)))
	(setq l (cdr l)))
      (set hooklist ret)
      ret))

(defun zephyr-lecture-buffer-create ()
  (save-excursion
    (setq zephyr-lecture-given t)
    (set-buffer (get-buffer-create "*zephyr-lecture*"))
    (insert (format "*** %s ***\n" (current-time-string)))
    (current-buffer)))

;; contributed by Michael Welsh Duggan <md5i@cs.cmu.edu>
(defun zephyr-quote-message (&optional location)
  "Quote the zephyr at LOCATION.  
If LOCATION is nil, then use message at point.  If the point is in the 
sending area, will use the previous message."
  (interactive)
  (let ((p (or location (point))))
    (goto-char p)
    (forward-line)
    (let* ((either-div (concat "^.+\\(" zephyr-receive-divider-regexp
			       "\\|" zephyr-send-divider-regexp "\\)"))
	   (end-msg (if (re-search-forward either-div (point-max) t)
			  (match-beginning 0)
		      (re-search-backward either-div (point-min) t)
		      (goto-char (match-beginning 0))
		      (match-beginning 0)))
	   (-- (goto-char (match-beginning 0)))
	   (begin-msg (if (re-search-backward either-div (point-min) t)
			  (goto-char (match-end 0))
			(point-min))))
      (goto-char (point-max))
      (re-search-backward either-div (point-min) t)
      (goto-char (match-end 0))
      (if (not (bolp))
	  (if (eobp)
	      (insert "\n")
	    (progn (forward-line) (beginning-of-line))))
      (let ((location (point)))
	(insert (buffer-substring begin-msg end-msg))
	(setq p (copy-marker (point)))
	(if (bolp)
	    (forward-line -1)
	  (beginning-of-line))
	(string-rectangle location (point) zephyr-quote-string)))
    (goto-char p)))

;; stolen from fancy-xmouse.el by Benjamin C. Pierce (bcp@cs.cmu.edu)
(defun zephyr-time-difference-in-seconds (time1 time2)
  (let* ((t1 (substring time1 11))
 	 (t2 (substring time2 11))
 	 (date1 (substring time1 0 11))
 	 (date2 (substring time2 0 11))
 	 (h1 (string-to-int (substring t1 0 2)))
 	 (h2 (string-to-int (substring t2 0 2)))
 	 (m1 (string-to-int (substring t1 3 5)))
 	 (m2 (string-to-int (substring t2 3 5)))
 	 (s1 (string-to-int (substring t1 6 8)))
 	 (s2 (string-to-int (substring t2 6 8)))
 	 (sec1 (+ (* 3600 h1) (* 60 m1) s1))
 	 (sec2 (+ (* 3600 h2) (* 60 m2) s2)))
    (+ (- sec2 sec1)
       (if (string-equal date1 date2)
 	   0
	 (* 3600 24) ; correction for passing midnight
	 ))))

(fset 'zephyr-buffer-substring-no-properties
      (symbol-function
       (if (fboundp 'buffer-substring-no-properties)
	   'buffer-substring-no-properties
	 'buffer-substring)))

(if (fboundp 'run-hook-with-args)
    (fset 'zephyr-run-hook-with-args (symbol-function 'run-hook-with-args))
  ;; Backwards-compatibility cruft for emacs18.
  (defun zephyr-run-hook-with-args (hook &rest args)
    "emulates the version 19 run-hook-with-args"
    (and (boundp hook)
	 (symbol-value hook)
	 (let ((value (symbol-value hook)))
	   (if (and (listp value) (not (eq (car value) 'lambda)))
	       (mapcar '(lambda (foo) (apply foo args))
		       value)
	     (apply value args))))))

(provide 'zephyr)

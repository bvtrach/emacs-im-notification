;;; bbcode-mode.el --- Major mode to edit bbcode files in Emacs
;;
;; Author: Bogdan Trach
;; Created: February, 2012
;;
;;     This program is free software: you can redistribute it and/or modify
;;     it under the terms of the GNU General Public License as published by
;;     the Free Software Foundation, either version 3 of the License, or
;;     (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public License
;;     along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; The hook code was written for jiggle.el by Will Mengarini

(require 'advice)
(require 'dbus)

;; customization menu setup
(defun im-notification-bus-safe-p (bus)
  (if (member bus `(:system :session))
      t nil))

(defgroup im-notification nil
  "Input method notification options."
  :group 'convenience)

(defcustom im-notification-bus :system
  "D-bus bus for notification. Session bus may be unavailable from Emacs-daemon."
  :group 'im-notification
  :type 'symbol
  :safe 'im-notification-bus-safe-p)

;; Definition of a hook
(defvar im-notification-buffer-switch-hook nil
  "Hook that runs any time the user switches buffers.
Deliberately ignores minibuffer since that has its own hooks.")

;;; Implementation of the hook:
(mapcar (function
         (lambda (f)
           (eval
            `(defadvice ,f (after run-im-notification-buffer-switch-hook act)
               "Implement im-notification-buffer-switch-hook."
               (run-hooks 'im-notification-buffer-switch-hook)))))
        '(bury-buffer
          kill-buffer
          other-window
          pop-to-buffer
          switch-to-buffer
          ))

(defun im-notification-get-name ()
  "Get short input method name (the one from the modeline)."
  (if current-input-method 
      (capitalize 
       (cadddr (find current-input-method 
		     input-method-alist 
		     :test (lambda (a b) (string-equal a (car b))))))
    "nil"))

(defun im-notification-send-current ()
  "Send dbus signal with current quail short name to WM. Some problems are possible with session bus."
  (dbus-send-signal 
   im-notification-bus
   dbus-service-emacs dbus-path-emacs dbus-service-emacs
   "imChanged" (im-notification-get-name)))

(defun im-notification-send-delete ()
  "Send dbus signal to WM, notifying it that no IM is active. Used when no window is active (window deactivation, etc.)"
  (dbus-send-signal 
   im-notification-bus
   dbus-service-emacs dbus-path-emacs dbus-service-emacs 
   "imChanged" "nil"))

(defun im-notification-setup ()
  "Setup hooks and a callback listener for notifications."
  (add-hook 'input-method-activate-hook 'im-notification-send-current)
  (add-hook 'input-method-inactivate-hook 'im-notification-send-delete)
  (add-hook 'im-notification-buffer-switch-hook 'im-notification-send-current)
  (add-hook 'window-configuration-change-hook 'im-notification-send-current)
  (add-hook 'delete-frame-functions 'im-notification-send-delete)
  (add-hook 'mouse-leave-buffer-hook 'im-notification-send-current)
  (dbus-register-signal 
   im-notification-bus nil "/org/awesome/im"
   "org.awesome.im" "imRequest" 'im-notification-request-handler))

(defun im-notification-check-session-bus ()
  "Check if emacs process has access to session bus."
  (if (find "DBUS_SESSION_BUS_ADDRESS" 
	    process-environment
	    :test 'string-match)
      t nil))

;; code for handling callback from window manager
(defun im-notification-request-handler (id)
  "If request is received, notify WM. Used when window is activated or deactivated, due to lack of some hooks in emacs."
  (im-notification-send-current))

(provide 'im-notification)

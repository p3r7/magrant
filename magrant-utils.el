


;; REQUIRES

(require 's)
(require 'dash)
(require 'tramp)
(require 'tablist)
(require 'transient)

(require 'magrant-core)



;; UTILS: BUFFERS

(defun magrant-utils-pop-to-buffer (name)
  "Like `pop-to-buffer', but suffix NAME with the host if on a remote host."
  (pop-to-buffer
   (if (file-remote-p default-directory)
       (with-parsed-tramp-file-name default-directory nil (concat name " - " host))
     name)))



;; UTILS: TABLIST / TRANSIENT

(defun magrant-utils-get-marked-items-ids ()
  "Get the id part of `tablist-get-marked-items'."
  (-map #'car (tablist-get-marked-items)))

(defun magrant-utils-ensure-items ()
  (when (null (magrant-utils-get-marked-items-ids))
    (user-error "This action cannot be used on an empty list")))

(defmacro magrant-utils-define-transient-command (name arglist &rest args)
  "Macro for building transient.el command NAME taking ARGLIST and ARGS content.
To use for commands that can target multiple entries at once from tabulated-list"
  `(define-transient-command ,name ,arglist
     ,@args
     (interactive)
     (magrant-utils-ensure-items)
     (transient-setup ',name)))

(defun magrant-utils-generic-actions-heading ()
  "Generate heading for transient.el command that can target multiple entries at once from tabulated-list"
  (let ((items (s-join ", " (magrant-utils-get-marked-items-ids))))
    (format "%s %s"
            (propertize "Actions on" 'face 'transient-heading)
            (propertize items        'face 'transient-value))))

(defun magrant-utils-get-transient-action ()
  (s-replace "-" " " (s-chop-prefix "magrant-" (symbol-name current-transient-command))))

(defun magrant-utils-generic-action (action args)
  (interactive (list (magrant-utils-get-transient-action)
                     (transient-args current-transient-command)))
  (--each (magrant-utils-get-marked-items-ids)
    (magrant-run-vagrant action args it))
  (tablist-revert))

;; REVIEW: transient.el must provide this in a more straightforward way
(defun magrant-utils-generic-action-with-item-prefix (name-arg-prefix &optional action args)
  (setq action (or action (magrant-utils-get-transient-action))
        args (or args (transient-args current-transient-command)))
  (--each (magrant-utils-get-marked-items-ids)
    (magrant-run-vagrant action args (concat name-arg-prefix it)))
  (tablist-revert))




(provide 'magrant-utils)

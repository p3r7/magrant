


;; REQUIRES

(require 'cl-lib)
(require 's)
(require 'dash)
(require 'tablist)
(require 'transient)

(require 'magrant-core)
(require 'magrant-utils)
(require 'magrant-faces)



;; VARS

(defgroup magrant-machine nil
  "vagrant machines customization group."
  :group 'magrant)

(defcustom magrant-machine-default-sort-key '("Id" . nil)
  "Sort key for vagrant machine.

This should be a cons cell (NAME . FLIP) where
NAME is a string matching one of the column names
and FLIP is a boolean to specify the sort order."
  :group 'magrant-box
  :type '(cons (choice (const "Id")
                       (const "Name")
                       (const "Provider")
                       (const "State")
                       (const "Directory"))
               (choice (const :tag "Ascending" nil)
                       (const :tag "Descending" t))))


;; FACES

(defun magrant-machine-state-face (statE)
  "Return the correct face according to STATE."
  (cond
   ((s-equals? "running" status)
    'magrant-face-status-up)
   ((s-equals? "poweroff" status)
    'magrant-face-status-down)
   ((s-equals? "saved" status)          ; suspended
    'magrant-face-status-other)
   (t
    'magrant-face-status-other)))


;; CLI CALLS

(defun magrant-machine-refresh ()
  "Refresh the machines list."
  (setq tabulated-list-entries (magrant-machine-entries)))

(defun magrant-machine-entries ()
  "Return the vagrant machines data for `tabulated-list-entries'."
  (let* ((data (magrant-run-vagrant "global-status"))
         (lines (s-split "\n" data t)))
    (cl-loop with i = 0
             for l in lines

             until (s-equals? (s-trim l) "")

             unless (< i 2)
             collect (magrant-machine-parse l)

             do (setq i (+ i 1)))))

(defun magrant-machine-parse (line)
  "Convert a LINE from \"vagrant global-status\" to a `tabulated-list-entries' entry."
  (let ((tab-line (apply #'vector
                         (-remove 's-blank?
                                  (s-split " " line)))))
    (list (aref tab-line 0) tab-line)))



;; MODE

(defvar magrant-machine-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" #'magrant-machine-help)

    (define-key map "v" #'magrant-machine-validate)

    (define-key map "p" #'magrant-machine-provision)
    (define-key map "U" #'magrant-machine-up)
    (define-key map "H" #'magrant-machine-halt)
    (define-key map "S" #'magrant-machine-suspend)

    (define-key map "b" #'magrant-machine-ssh)

    (define-key map "D" #'magrant-machine-destroy)
    (define-key map "l" #'magrant-machine-list)
    map)
  "Keymap for `magrant-machine-mode'.")

(define-derived-mode magrant-machine-mode tabulated-list-mode "Machines Menu"
  "Major mode for handling a list of vagrant machines."
  (setq tabulated-list-format [("Id" 8 t)("Name" 15 t)("Provider" 15 t)
                               ("State" 10 t)
                               ("Directory" 30 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key magrant-machine-default-sort-key)
  (add-hook 'tabulated-list-revert-hook #'magrant-machine-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))



;; TRANSIENT: ENTRY POINT

(define-transient-command magrant-machine-help ()
  "Help transient for vagrant machines."
  ["Vagrant machines help"
   ("v" "Validate" magrant-machine-validate)
   ("p" "Provision" magrant-machine-provision)

   ("U" "Up (start)" magrant-machine-up)
   ("H" "Halt" magrant-machine-halt)
   ("S" "Suspend" magrant-machine-suspend)

   ("b" "SSH" magrant-machine-ssh)
   ("D" "Destroy" magrant-machine-destroy)
   ("l" "List" magrant-machine-list)])

(define-transient-command magrant-machine-list ()
  "Transient for listing machines."
  :man-page "magrant-machine-list"
  ["Actions"
   ;; NB: `magrant-box-refresh' called by hook, see `magrant-box-mode'
   ("l" "List" tablist-revert)])



;; TRANSIENT: VALIDATE

(magrant-utils-define-transient-command
 magrant-machine-validate ()
 "Transient for updating boxes."
 :man-page "magrant-box-update"
 ["Filter arguments"
  ("-p" "Ignore Provider" "--ignore-provider")]
 [:description magrant-utils-generic-actions-heading
               ("v" "Validate" magrant-machine--validate-action)])

(defun magrant-machine--validate-action ()
  "Validate selected boxes."
  (interactive)
  ;; TODO: need to let `default-directory' to "Directory" col and run vagrant validate here...
  (message "Not yet implemented"))



;; TRANSIENT: PROVISION

(defun magrant-machine-provision ()
  (interactive)
  (message "Not yet implemented"))



;; TRANSIENT: START / STOP

(defun magrant-machine-up ()
  (interactive)
  (message "Not yet implemented"))

(defun magrant-machine-halt ()
  (interactive)
  (message "Not yet implemented"))

(defun magrant-machine-suspend ()
  (interactive)
  (message "Not yet implemented"))



;; TRANSIENT: CONNECT

(defun magrant-machine-ssh ()
  (interactive)
  (message "Not yet implemented"))



;; TRANSIENT: DESTROY

(defun magrant-machine-destroy ()
  (interactive)
  (message "Not yet implemented"))



;; COMMAND

(defun magrant-machines ()
  "List vagrant machines."
  (interactive)
  (magrant-utils-pop-to-buffer "*vagrant-machines*")
  (magrant-machine-mode)
  (tablist-revert))




(provide 'magrant-machine)

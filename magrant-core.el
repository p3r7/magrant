


;; REQUIRES

(require 's)
(require 'dash)
(require 'transient)
(require 'friendly-shell-command)



;; VARS

(defgroup magrant nil
  "Vagrant wrapper customization group."
  :group 'convenience)

(defcustom magrant-command "vagrant"
  "The vagrant binary."
  :group 'magrant
  :type 'string)


;; TRANSIENT

;;;###autoload (autoload 'magrant "magrant" nil t)
(define-transient-command magrant ()
  "Transient for vagrant."
  :man-page "magrant"
  ["Vagrant"
   ("m" "Machines"   magrant-machines)
   ("b" "Boxes"      magrant-boxes)]
  ;; ["Other"
  ;;  ("C" "Cloud"      magrant-cloud)]
  )



;; UTILS

(defun magrant-run-vagrant (action &rest args)
  "Execute \"`magrant-command' ACTION ARGS\"."
  (let* ((command (s-join " " (-remove 's-blank? (-flatten (list magrant-command action args))))))
    (message command)
    (friendly-shell-command-to-string command)))

(defun magrant-run-vagrant-async (action buffer &rest args)
  "Execute \"`magrant-command' ACTION ARGS\"."
  (let* ((command (s-join " " (-remove 's-blank? (-flatten (list magrant-command action args))))))
    (message command)
    (friendly-shell-command-async command
                                  :output-buffer buffer)))




(provide 'magrant-core)

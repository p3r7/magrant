


;; REQUIRES

(require 'magrant-core)



;; FACES

(defgroup magrant-faces nil
  "Magrant faces."
  :group 'magrant
  :group 'faces)

(defface magrant-face-status-up
  '((t :foreground "Green"))
  "Face used when the status is up."
  :group 'magrant-faces)

(defface magrant-face-status-down
  '((t :foreground "Red"))
  "Face used when the status is down"
  :group 'magrant-faces)

(defface magrant-face-status-other
  '((t :foreground "Gold"))
  "Face used when the status is not up/down."
  :group 'magrant-faces)




(provide 'magrant-faces)

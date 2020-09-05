(defun anapython () (interactive)
  (let ((python-shell-interpreter "mypython"))
    (call-interactively 'run-python)))

(defun anapython_minitorch () (interactive)
  (let ((python-shell-interpreter "mypython_minitorch"))
    (call-interactively 'run-python)))

;; (defun anapython_minitorch () (interactive)
;;   (let ((python-shell-interpreter "mypython_minitorch"))
;;     (run-python)))

(setq testmeout "ok")


(use-package elpy
  :ensure t
  :init
  (elpy-enable))

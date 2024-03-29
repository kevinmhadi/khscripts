(load-file "~/.emacs.d/lisp/essh.el")
(load-file "~/.emacs.d/lisp/emacs.func.el")
;; (load-file "~/blog.lisp")
(load-file "~/.emacs.d/lisp/copypaste.lisp")

(global-set-key (kbd "C-c f") `ess-function-name)
(global-set-key (kbd "C-c o") `ess-swv-knit)

(add-hook 'sh-mode-hook 'highlight-indent-guides-mode)

(defun unstick-ansi-color-codes ()
  (interactive)
  (end-of-buffer)
  (insert "echo -e \"\033[m\"")
  (comint-send-input nil t))

(add-hook 'inferior-ess-mode-hook
	  (lambda ()
	    (setq eldoc-mode nil)
	    (setq ess-use-eldoc nil)
	    (call-interactively 'ess-disable-smart-S-assign t)))


(load "~/software/ess-16.04/lisp/ess-site")
(add-hook 'ess-noweb-mode-hook 'rknit-compile-and-print-pdflatex)
(add-hook 'ess-noweb-font-lock-mode-hook 'rknit-compile-and-print-pdflatex)
(add-hook 'ess-mode-hook
	  (lambda ()
	    (setq eldoc-mode nil)
	    (setq ess-use-eldoc nil) 
	    (call-interactively 'ess-disable-smart-S-assign t)))


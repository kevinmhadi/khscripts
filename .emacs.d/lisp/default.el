(load "~/software/ess-16.04/lisp/ess-site")

(load-file "~/.emacs.d/lisp/essh.el")
(load-file "~/.emacs.d/lisp/emacs.func.el")
;; (load-file "~/blog.lisp")
(load-file "~/.emacs.d/lisp/copypaste.lisp")

(global-set-key (kbd "C-c f") `ess-function-name)
(global-set-key (kbd "C-c o") `ess-swv-knit)

(add-hook 'ess-noweb-mode-hook 'rknit-compile-and-print-pdflatex)
(add-hook 'ess-noweb-font-lock-mode-hook 'rknit-compile-and-print-pdflatex)
(add-hook 'ess-mode-hook
	  (lambda ()
	    (setq eldoc-mode nil)
	    (setq ess-use-eldoc nil)))



(add-hook 'inferior-ess-mode-hook
	  (lambda ()
	    (setq eldoc-mode nil)
	    (setq ess-use-eldoc nil)))

(add-hook 'sh-mode-hook 'highlight-indent-guides-mode)

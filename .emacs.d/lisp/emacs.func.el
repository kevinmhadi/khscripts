(require 'essh)                                                    ;;
(defun essh-sh-hook ()                                             ;;
  (define-key sh-mode-map "\C-c\C-r" 'pipe-region-to-shell)        ;;
  (define-key sh-mode-map "\C-c\C-b" 'pipe-buffer-to-shell)        ;;
  (define-key sh-mode-map "\C-c\C-j" 'pipe-line-to-shell)          ;;
  (define-key sh-mode-map "\C-c\C-n" 'pipe-line-to-shell-and-step) ;;
  (define-key sh-mode-map "\C-c\C-f" 'pipe-function-to-shell)      ;;
  (define-key sh-mode-map "\C-c\C-d" 'shell-cd-current-directory)) ;;
(add-hook 'sh-mode-hook 'essh-sh-hook)  


(defun global-disable-mode (mode-fn)
  "Disable `MODE-FN' in ALL buffers."
    (interactive "a")
      (dolist (buffer (buffer-list))
          (with-current-buffer buffer
  	        (funcall mode-fn -1))))


;; (defun disable-eldoc ()
;;   "run disable-eldoc"
;;   (interactive)
;;   (if (not (active-minibuffer-window))
;;       (eldoc-mode -1)))

;(add-hook 'buffer-list-update-hook 'disable-eldoc)
  
;; (setq scroll-step 1)
;; (setq scroll-conservatively 10000)

;; defaults for scrolling
;; (setq scroll-step 0)
;; (setq scroll-conservatively 0)


;; (unless window-system
;;   (when (getenv "DISPLAY")
;;     ;; Callback for when user cuts
;;     (defun xsel-cut-function (text &optional push)
;;       ;; Insert text to temp-buffer, and "send" content to xsel stdin
;;       (with-temp-buffer
;; 	(insert text)
;; 	;; I prefer using the "clipboard" selection (the one the
;; 	;; typically is used by c-c/c-v) before the primary selection
;; 	;; (that uses mouse-select/middle-button-click)
;; 	(call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
;;     ;; Call back for when user pastes
;;     (defun xsel-paste-function()
;;       ;; Find out what is current selection by xsel. If it is different
;;       ;; from the top of the kill-ring (car kill-ring), then return
;;       ;; it. Else, nil is returned, so whatever is in the top of the
;;       ;; kill-ring will be used.
;;       (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
;; 	(unless (string= (car kill-ring) xsel-output)
;; 	  xsel-output )))
;;     ;; Attach callbacks to hooks
;;     (setq interprogram-cut-function 'xsel-cut-function)
;;     (setq interprogram-paste-function 'xsel-paste-function)
;;     ;; Idea from
;;     ;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
;;     ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
;;     ))


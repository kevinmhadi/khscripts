;; (xterm-mouse-mode t)
;; This is only needed once, near the top of the file
;; (eval-when-compile
;;   ;; Following line is not needed if use-package.el is in ~/.emacs.d
;;   (require 'use-package))

(setq-default fill-column 999999999999999999)

(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(setq org-agenda-files (list "c:/Users/kevin/Google Drive/org_blog.org"))
;; (package-refresh-contents)

(add-hook 'org-mode-hook
	  (lambda ()
	    (local-unset-key (kbd "C-c a"))
	    (local-set-key (kbd "C-c a") `org-agenda)))

(add-hook 'sh-mode-hook 'highlight-indent-guides-mode)
(add-hook 'bat-mode-hook 'highlight-indent-guides-mode)

;; (defun run-powershell ()
;;   "Run powershell"
;;   (interactive)
;;   (async-shell-command "c:/windows/system32/WindowsPowerShell/v1.0/powershell.exe -Command -"
;;                nil
;;                nil))



(defun powershell (&optional buffer)
  "Launches a powershell in buffer *powershell* and switches to it."
  (interactive)
  (let ((buffer (or buffer "*powershell*"))
    (powershell-prog "c:\\windows\\system32\\WindowsPowerShell\\v1.0\\powershell.exe"))
    (make-comint-in-buffer "shell" "*powershell*" powershell-prog)
    (switch-to-buffer buffer)))


;; (load-file "~/blog.lisp")
;; (desktop-save-mode 1)
;; (add-to-list 'load-path "~/ess-16.10/lisp")
;; (load "ess-site")
(setq lazy-highlight-cleanup t)
(setq lazy-highlight-max-at-a-time nil)
(setq lazy-highlight-initial-delay 0)
;; (setq transient-mark-mode nil)
(transient-mark-mode 1)
(blink-cursor-mode -1)

;; (ess-toggle-underscore nil)
;; (setq ess-smart-S-assign-key nil)
;; (setq ess-smart-S-assign-key "_")
;; (ess-toggle-S-assign nil)
(set-face-background 'lazy-highlight nil)
(set-face-foreground 'lazy-highlight "red2")
(set-face-attribute 'lazy-highlight nil
		    :weight 'bold)
;(setq ess-swv-processor "knitr")
;(setq ess-swv-pdflatex-commands `("pdflatex"))
					;(setq ess-eval-visibly-p `nowait)
;; (blink-cursor-mode 0)
;; (blink-cursor-mode 1)

(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-input-sender-no-newline t)

;(setq redisplay-dont-pause nil)



(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))

(defun forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
	((looking-back "\\s)" 1) (backward-sexp arg))
	;; Now, try to succeed from inside of a bracket
	((looking-at "\\s)") (forward-char) (backward-sexp arg))
	((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))


;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph    
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

;; (defun insert-current-date ()
;;    (interactive)
;;    (insert (format-time-string "#' %A, %b %d, %Y %r")))

(defun insert-current-date ()
   (interactive)
   (insert (concat "#' "  (concat (getenv "USERNAME")) " "))
   (insert (format-time-string "%A, %B %d, %Y, Week %V, %T"))
   )


(defun daytime-colors ()
  (interactive)
  (load "~/.emacs.d/col_profile_white"))


;; (defun nighttime-colors ()
;;   (interactive)
;;   (load "~/dev/config/.emacs_colors_marcin"))

(defun my-colors ()
  (interactive)
  (load "~/.emacs.d/col_profile"))

;; (global-set-key (kbd "C-c N") `nighttime-colors)


(global-set-key (kbd "C-c E") `daytime-colors)

(global-set-key (kbd "C-c N") `my-colors)


(global-set-key (kbd "C-c d") `insert-current-date)
(global-set-key (kbd "C-c ;") `forward-or-backward-sexp)
;(global-set-key (kbd "C-c ;") `goto-match-paren)
;(global-set-key (kbd "C-c f") `ess-function-name)
;(global-set-key (kbd "C-c P") `rknit-pdf)
;(global-set-key (kbd "C-c p") `rknit-pdfonly)
;(global-set-key (kbd "C-c o") `ess-swv-knit)
(global-set-key (kbd "C-c i") `indent-region)
(global-set-key (kbd "C-c ]") `comment-region)
(global-set-key (kbd "C-c [") `uncomment-region)
(global-set-key (kbd "C-c ?") `shell)
(global-set-key (kbd "C-?") `shell)
(global-set-key (kbd "C-c r") `rename-buffer)
(global-set-key (kbd "C-c t") `revert-buffer)
(global-set-key (kbd "C-c u") `overwrite-mode)
;(global-set-key (kbd "C-c p") `run-python)
;(global-set-key (kbd "C-c j") `julia)
(global-set-key (kbd "ESC <up>") `previous-buffer)
(global-set-key (kbd "ESC <down>") `next-buffer)
(global-set-key (kbd "M-<up>") `previous-buffer)
(global-set-key (kbd "M-<down>") `next-buffer)
(global-set-key (kbd "<select>") `windmove-up)
(global-set-key [drag-mouse-0] `mouse-set-region)
(windmove-default-keybindings)



(define-key esc-map "G" 'goto-line)             ; Esc-G runs the goto-line
						; function.

(define-key ctl-x-map "t" 'transpose-lines)     ; Ctrl-x t runs the
						; transpose-lines function.

(global-font-lock-mode t)

;(set-background-color "grey11")
;(set-foreground-color "white")
;(set-cursor-color "white")
;(set-frame-font "-*-fixed-*-*-*--11-*-*-*-c-*-*-*")
;(add-to-list 'default-frame-alist '(font . "6x13"))
;(add-to-list 'default-frame-alist '(foreground-color . "white"))
;(add-to-list 'default-frame-alist '(background-color . "grey11"))
;(add-to-list 'default-frame-alist '(cursor-color . "white"))

(set-background-color "grey14")
;; (set-background-color "grey15")ORIGINAL
(set-foreground-color "grey80")
(set-cursor-color "white")
;; (set-frame-font "-*-fixed-*-*-*--11-*-*-*-c-*-*-*")
;; (add-to-list 'default-frame-alist '(font . "6x13"))
(set-frame-font "Consolas-12:antialias=false:hinting=false")
;; (add-to-list 'default-frame-alist '(font . "Andale Mono-11:antialias=false:hinting=false" ))

(add-to-list 'default-frame-alist '(foreground-color . "grey80"))
(add-to-list 'default-frame-alist '(background-color . "grey20"))
(add-to-list 'default-frame-alist '(cursor-color . "white"))
;; (set-face-background 'default "grey14")ORIGINAL
(set-face-background 'default "grey14")
(set-face-foreground 'default "grey80")

(set-face-background 'mode-line-inactive "grey50")
(set-face-foreground 'mode-line-inactive "grey20")


(set-face-foreground 'font-lock-string-face "orange")
(set-face-foreground 'font-lock-comment-face "wheat")
;(set-face-foreground 'modeline-buffer-id "black")
;(set-face-background 'modeline-buffer-id "white")
(set-face-foreground 'font-lock-keyword-face "aquamarine")
(set-face-foreground 'font-lock-type-face "yellow")
(set-face-foreground 'font-lock-constant-face "yellow")
;(set-face-foreground 'font-lock-preprocessor-face "yellow")
;(set-face-foreground 'modeline-mousable "black")
;(set-face-background 'modeline-mousable "white")
;(set-face-background 'buffers-tab "grey30")
;(set-face-background 'toolbar "grey75")
;(set-face-background 'gui-element "grey75")

;; (prefer-coding-system 'utf-8)
(setq explicit-bash.exe-args '("--login" "-i"))

(defun git-bash () (interactive)
  (let ((explicit-shell-file-name "C:/Program Files/Git/bin/bash.exe"))
    (call-interactively 'shell)))

;; (defun anapython () (interactive)
;;   (let ((python-shell-interpreter "mypython"))
;;     (call-interactively 'run-python)))

;; (defun anapython_minitorch () (interactive)
;;   (let ((python-shell-interpreter "mypython_minitorch"))
;;     (run-python)))

;(require 'ess-rutils)


(setq require-final-newline t)                  ; Make sure file always
						; ends with a newline.
;(x-set-font "-adobe-courier-medium-r-*-*-20-*-*-*-*-*-*-*")

;(setq default-major-mode 'text-mode)            ; Default mode is text-mode.

(setq text-mode-hook                            ; Enable auto-fill-mode
 '(lambda () (auto-fill-mode 1)))               ; whenever using text-mode.

(setq delete-auto-save-files t)                 ; Delete unnecessary
						; auto-save files.

(defun my-exit-from-emacs()
  (interactive)
  ( if (yes-or-no-p "Do you want to exit ")
      (save-buffers-kill-emacs)))
(global-set-key "\C-x\C-c" 'my-exit-from-emacs)

;(defun small-font()
;  (interactive)
 ; (x-set-font "-adobe-courier-bold-r-*-*-12-*-*-*-*-*-*-*"))

;(defun large-font()
 ; (interactive)
  ;(x-set-font "-adobe-courier-medium-r-*-*-14-*-*-*-*-*-*-*"))




(defun adjust-window-up()
  (interactive)
  (scroll-up 2))

(defun adjust-window-down()
  (interactive)
  (scroll-down 2))

; John Carr <jfc@athena.mit.edu>, Feb 1989
; On both vax and rt the function keys emit "ESC [ # ~"
; The keys map to values of "#" as follows:
; F1            11
; ...
; F5            15
; F6            17
; ...
; F10           21
; F11           23      (RT only; this is "escape" on the vax)
; F12 24
; F13           25      (Vax only; no such key on RT)
; F14           26      (Vax only; no such key on RT)

; First, define an empty keymap to hold the bindings
;(defvar fnkey-map (make-sparse-keymap) "Keymap for function keys")

; Second, bind it to ESC-[ (which is the prefix used on the function keys.
; #(define-key esc-map "[" fnkey-map)

; Third, bind functions to the keys.  Note that you must use the internal
; emacs-lisp function names, which are usually, but not always, the same as
; the name used to invoke the command via M-x.

; One key is bound to a non-standard function: "mail-sig".  This ; is a
; keyboard macro I have defined to sign my mail with a single keystroke.
; When a keyboard macro is invoked, the effects are as if you had typed all
; the characters that make up its definition.  To define a keyboard macro,
; do something like this:

(fset 'mail-sig
  " Lois Bennett <lois@athena.mit.edu>")

; (set-frame-font "-misc-fixed-medium-r-semicondensed--11-*-*-*-c-60-iso8859-1")


; The "^M" is a control character, type C-q C-m to insert it.
; This macro can be invoked by typing M-x and its name, or with F2.

(defun set-dir-switch-n()
(interactive)
(setq dired-listing-switches "-Al"))

(defun set-dir-switch-t()
(interactive)
(setq dired-listing-switches "-Alt"))


; don't ask to follow git controlled links
(setq vc-follow-symlinks nil)
(setq org-confirm-babel-evaluate nil)

(load "~/.emacs.d/col_profile")

(require 'package) ; you may already have this line
;; (add-to-list 'package-archives
;;              '("cselpa" . "https://elpa.thecybershadow.net/packages/"))

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; (require 'term-keys)
;; (term-keys-mode t)


;; (require 'term-keys-xterm)
;; (with-temp-buffer
;;   (insert (term-keys/xterm-script))
;;   (write-region (point-min) (point-max) "~/launch-xterm-with-term-keys.sh"))


;; (require 'term-keys-terminal-app)
;; (with-temp-buffer
;;   (insert (term-keys/terminal-app-keymap-xml))
;;   (append-to-file (point-min) (point-max) "~/term-keys.xml"))


;; (global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
;; (global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
;; (global-set-key (kbd "S-C-<left>") 'shrink-window)
;; (global-set-key (kbd "S-C-<right>") 'enlarge-window)

;; Smooth scrolling
(setq scroll-step            1
            scroll-conservatively  10000)

;; Default settings
;; (setq scroll-step            0
;;             scroll-conservatively  0)

;; custom funs
(defun back-window ()
  (interactive)
  (other-window -1))


;; Moving cursor quickly (cycling) without moving screen
;; C-l moves the screen from top to bottom
(global-unset-key (kbd "M-u"))		; don't need this... this converts following word to uppercase - see M-x upcase-region
(global-unset-key (kbd "M-l"))		; don't need this keybinding this convers following world to lowercase - see M-x lowercase-region
(global-unset-key (kbd "M-o"))
(global-set-key (kbd "M-u") `move-to-window-line-top-bottom)
(global-set-key (kbd "M-o") `other-window)
(global-set-key (kbd "M-l") `back-window)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; (require 'eval-in-repl)
;; (require 'eval-in-repl-shell)
;; (add-hook 'sh-mode-hook
;;           '(lambda()
;;              (local-set-key (kbd "C-<return>") 'eir-eval-in-shell)))

;; Good hotkeys:
;; M-;	parentheses
;; M-j	newline with same comment prefix
;; C-x .	set fill prefix
;; M-i	force insert tab
;; M-a	move back a paragraph
;; M-e	move forward a paragraph
;; M-r	when on the command line of active process buffer: regexp back search
;; M-h	selects a whole paragraph? use with transient-mark-mode enabled

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(menu-bar-mode nil)
 '(package-check-signature nil)
 '(package-selected-packages
   (quote
    (use-package highlight-indent-guides elpy el-get powershell edit-server-htmlize edit-server org)))
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "grey14" :foreground "grey80" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "outline" :family "Consolas"))))
 '(mode-line-buffer-id ((t (:background "grey80" :foreground "grey20" :inverse-video t))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey25" :foreground "grey45" :box (:line-width -1 :color "grey40") :weight light)))))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)


(add-to-list 'load-path "~/.emacs.d/lisp")
;; (global-set-key (kbd "C-c N") `nighttime-colors)
;; (global-set-key (kbd "C-c D") `daytime-colors)

;; (menu-bar-mode 0)

;; use this block if something breaks with C-g
;; (global-set-key (kbd "C-^") `keyboard-quit)
;; default is (set-input-mode t nil 0 7)
;; (set-input-mode t nil 0 30)
;; (global-set-key (kbd "C-g") `keyboard-quit)

(global-set-key (kbd "C-c d") `insert-current-date)
(global-set-key (kbd "C-c ;") `forward-or-backward-sexp)
;(global-set-key (kbd "C-c ;") `goto-match-paren)
(global-set-key (kbd "C-c P") `rknit-pdf)
(global-set-key (kbd "C-c p") `rknit-pdfonly)
(global-set-key (kbd "C-c i") `indent-region)
(global-set-key (kbd "C-c ]") `comment-region)
(global-set-key (kbd "C-c [") `uncomment-region)
(global-set-key (kbd "C-c ?") `shell)
(global-set-key (kbd "C-?") `shell)
(global-set-key (kbd "C-c r") `rename-buffer)
(global-set-key (kbd "C-c t") `revert-buffer)
(global-set-key (kbd "C-c u") `overwrite-mode)
(global-set-key (kbd "C-c p") `run-python)
(global-set-key (kbd "C-c j") `julia)
(global-set-key (kbd "ESC <up>") `previous-buffer)
(global-set-key (kbd "ESC <down>") `next-buffer)
(global-set-key (kbd "M-<up>") `previous-buffer)
(global-set-key (kbd "M-<down>") `next-buffer)
(global-set-key (kbd "C-c '") `check-parens)
(global-set-key [drag-mouse-0] `mouse-set-region)
;; (global-set-key [left-margin drag-mouse-1] `mouse-drag-vertical-line)
;; (global-set-key [left-divider down-mouse-1] `mouse-drag-vertical-line)
;; (global-set-key [left-margin down-mouse-1] `mouse-drag-vertical-line)
;; (global-set-key [left-margin mouse-movement] `mouse-drag-vertical-line)
;; (global-set-key [right-divider down-mouse-1] `mouse-drag-vertical-line)
;; (global-set-key (kbd "ESC s") `check-parens)
(windmove-default-keybindings)
(blink-cursor-mode 1)
(setq blink-cursor-blinks -1)
;; (blink-cursor-mode t)
;; (ansi-color-for-comint-mode-filter)

(setq-default cursor-type '(bar . 1))

(setq ansi-color-for-comint-mode `filter)

;; (setq inferior-ess-mode-hook nil)
;; (add-hook 'inferior-ess-mode-hook 'turn-on-font-lock)
;; (add-hook 'inferior-ess-mode-hook 'ess-S-mouse-me-menu-commands)




(define-key esc-map "G" 'goto-line)             ; Esc-G runs the goto-line
						; function.

(define-key ctl-x-map "t" 'transpose-lines)     ; Ctrl-x t runs the
						; transpose-lines function.

(global-font-lock-mode t)


(defun daytime-colors ()
  (interactive)
  (load "~/.emacs.d/col_profile_white"))

(defun my-colors ()
  (interactive)
  (load "~/.emacs.d/col_profile"))

(defun deep-colors ()
  (interactive)
  (load "~/.emacs.d/col_profile_deep"))


(global-set-key (kbd "C-c N") `my-colors)
(global-set-key (kbd "C-c W") `deep-colors)
(global-set-key (kbd "C-c E") `daytime-colors)


;; (defun nighttime-colors ()
;;   (interactive)
;;   (load "~/dev/config/.emacs_colors_marcin"))


;(set-background-color "grey11")
;(set-foreground-color "white")
;(set-cursor-color "white")
;(set-frame-font "-*-fixed-*-*-*--11-*-*-*-c-*-*-*")
;(add-to-list 'default-frame-alist '(font . "6x13"))
;(add-to-list 'default-frame-alist '(foreground-color . "white"))
;(add-to-list 'default-frame-alist '(background-color . "grey11"))
;(add-to-list 'default-frame-alist '(cursor-color . "white"))

;; (set-background-color "grey14")
;; ;; (set-background-color "grey15")ORIGINAL
;; (set-foreground-color "grey80")
(set-cursor-color "white")
;; (set-frame-font "-*-fixed-*-*-*--11-*-*-*-c-*-*-*") ;; original line
;; (set-frame-font "-*-fixed-*-*-*--13-*-*-*-c-*-*-*")
;; (set-frame-font "-*-fixed-*-*-*--13-*-*-*-c-*-*-*")
;; (set-frame-font "DejaVu Sans Mono-11")
(set-frame-font "Fixed-13")
;; (set-frame-font "Courier-9")
;; (add-to-list 'default-frame-alist '(font . "6x13"))
;; (add-to-list 'default-frame-alist '(foreground-color . "white"))
;; (add-to-list 'default-frame-alist '(background-color . "grey20"))
;; (add-to-list 'default-frame-alist '(cursor-color . "white"))
;; ;; (set-face-background 'default "grey14")ORIGINAL
;; (set-face-background 'default "grey14")

;; (set-face-foreground 'default "grey80")

;; (set-face-background 'mode-line-inactive "grey50")
;; (set-face-foreground 'mode-line-inactive "grey20")


;; (set-face-foreground 'font-lock-string-face "orange")
;; (set-face-foreground 'font-lock-comment-face "wheat")
;; ;(set-face-foreground 'modeline-buffer-id "black")
;; ;(set-face-background 'modeline-buffer-id "white")
;; (set-face-foreground 'font-lock-keyword-face "aquamarine")
;; (set-face-foreground 'font-lock-type-face "yellow")
;; (set-face-foreground 'font-lock-constant-face "yellow")
;(set-face-foreground 'font-lock-preprocessor-face "yellow")
;(set-face-foreground 'modeline-mousable "black")
;(set-face-background 'modeline-mousable "white")
;(set-face-background 'buffers-tab "grey30")
;(set-face-background 'toolbar "grey75")
					;(set-face-background 'gui-element "grey75")

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
(setq hscroll-step 1)

;; Default settings
;; (setq scroll-step            0
;;       scroll-conservatively  0)
;; (setq hscroll-step 0)

;; custom funs
(defun back-window ()
  (interactive)
  (other-window -1))



(setq split-height-threshold 1)
(setq split-width-threshold 1)
;; (setq pop-up-windows nil)


;; Moving cursor quickly (cycling) without moving screen
;; C-l moves the screen from top to bottom
(global-unset-key (kbd "M-u"))		; don't need this... this converts following word to uppercase - see M-x upcase-region
(global-unset-key (kbd "M-l"))		; don't need this keybinding this convers following world to lowercase - see M-x lowercase-region
(global-unset-key (kbd "M-o"))
(global-set-key (kbd "M-u") `move-to-window-line-top-bottom)
(global-set-key (kbd "M-o") `other-window)
(global-set-key (kbd "M-l") `back-window)


(global-unset-key (kbd "C-c M-o"))

(global-set-key (kbd "C-c M-o") nil)

;; (global-set-key (kbd "C-c w") `point-to-register)
;; (global-set-key (kbd "C-c e") `jump-to-register)

(global-unset-key (kbd "C-c w"))
(global-unset-key (kbd "C-c e"))

(global-set-key (kbd "C-c m") `point-to-register)
(global-set-key (kbd "C-c ,") `jump-to-register)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(defun toggle-window-dedicated ()
    "Control whether or not Emacs is allowed to display another
buffer in current window."
    (interactive)
    (message
     (if (let (window (get-buffer-window (current-buffer)))
	   (set-window-dedicated-p window (not (window-dedicated-p window))))
	 "%s: Can't touch this!"
       "%s is up for grabs.")
        (current-buffer)))

;; (fset 'point_e
;;       (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("r e" 0 "%d")) arg)))

;; (fset 'point_r
;;       (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("r r" 0 "%d")) arg)))


;; (fset 'jump_e
;;    (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("rje" 0 "%d")) arg)))

;; (fset 'jump_r
;;    (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("rjr" 0 "%d")) arg)))

;; (global-set-key (kbd "C-c q") `point_e)
;; (global-set-key (kbd "C-c w") `point_r)

;; (global-set-key (kbd "C-c a") `jump_e)
;; (global-set-key (kbd "C-c s") `jump_r)


;; (define-key inferior-ess-mode-map (kbd "C-c M-o") nil)
;; (define-key inferior-ess-mode-map (kbd "C-c M-o") `comint-clear-buffer) ;; default

;; Good hotkeys:
;; M-;	parentheses
;; M-j	newline with same comment prefix
;; C-x .	set fill prefix
;; M-i	force insert tab
;; M-a	move back a paragraph
;; M-e	move forward a paragraph
;; M-r	when on the command line of active process buffer: regexp back search
;; M-h	selects a whole paragraph? use with transient-mark-mode enabled
;; C-h b	help listing -- all possible keybindings, same as M-x describe-bindings
;; C-h k	describe a specific keybinding, same as M-x describe-key
;; C-h v	describe a lisp variable, same as M-x describe-variable



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

(defun insert-current-date ()
   (interactive)
   (insert (concat "#' "  (concat (getenv "USER")) " "))
   (insert (format-time-string "%A, %b %d, %Y, Week %V, %r"))
   )

(defun adjust-window-up()
  (interactive)
  (scroll-up 2))

(defun adjust-window-down()
  (interactive)
  (scroll-down 2))




;(load "~/Software/ess-5.11/lisp/ess-site")
;(load "~/software/ess-15.03-2/lisp/ess-site")

(setq inferior-julia-program-name "/nfs/sw/julia/julia-0.4.6/bin/julia")
(setq lazy-highlight-cleanup t)
(setq lazy-highlight-max-at-a-time nil)
(setq lazy-highlight-initial-delay 0)
;; (setq transient-mark-mode t)
;; (setq ess-smart-S-assign-key "_")
;; (setq ess-smart-S-assign-key nil)
;; (ess-toggle-S-assign nil)
;; (setq transient-mark-mode nil)
(setq transient-mark-mode t)
(set 'server-name "daemon")
;; (set 'debug-on-quit nil)
(set 'pop-up-windows t)
;; (set 'tooltip-mode nil)
;; (set 'tool-bar-mode nil)
(set-face-background 'lazy-highlight nil)
(set-face-foreground 'lazy-highlight "red2")
(set-face-attribute 'lazy-highlight nil
		    :weight 'bold)
(setq ess-swv-processor "knitr")
(setq ess-swv-pdflatex-commands `("pdflatex"))
(setq ess-eval-visibly-p `nowait)


(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-input-sender-no-newline t)

;(setq redisplay-dont-pause nil)

(defun ess-function-name ()
  "Print the name of the current function in the minibuffer.
   Based on ess-beginning-of-function"
  (interactive)
  (let ((init-point (point))
	beg end done)
    ;; First search for the start of the function definition
    ;; in case we're sitting in a function header:
    ;; at most end of next line
    (if (search-forward "(" (ess-line-end-position 2) t)
	(forward-char 1))
    (while (not done)
      (if (re-search-backward ess-function-pattern (point-min) t)
	  nil
	(goto-char init-point)
	(error "Point is not in a function."))

      (setq beg (point))
      ;; The point is now at the start of the function name
      (let (word regexp point end)
	(setq point (point))
	; Look forward for one of '" ' '"_' '"<' ' ' '_ <'
	(while (not (looking-at "\"*[ _<]"))
	  (forward-char 1))
	(setq end (point))
	(goto-char point)
	; Look backward for the start of function name, excluding quote
	(while (and (not (bobp)) (looking-at "\\w"))
	  (backward-char 1))
	(or (looking-at "\\w")
	    (forward-char 1))
	(and (= (point) end)
	     (error "not looking at a word"))
	(setq word (buffer-substring (point) end))
	(goto-char init-point)
	(error word)
	)
      ;; current function must begin and end around point
      (setq done (<= beg init-point)))
    beg))

(defun rknit-pdf ()
  (interactive)
  (ess-swv-knit)
  (sit-for 2)
  (ess-swv-PDF "pdflatex")
  )

(defun rknit ()
  (interactive)
  (ess-swv-knit)
  (sit-for 2)
  (ess-swv-PDF "pdflatex")
)

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

(defun rknit-pdfonly ()
  (interactive)
  (ess-swv-PDF "pdflatex")
)


;; (defun insert-current-date ()
;;    (interactive)
;;    (insert (format-time-string "#' %A, %b %d, %Y %r")))

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


(setq explicit-shell-file-name "/bin/bash")
(setq shell-command-switch "-ic")

(load "~/.emacs.d/col_profile")

;(setq explicit-shell-file-name "/usr/bin/tcsh")

;(setq lazy-highlight-face 'lazy-highlight2)


;; (define-key fnkey-map "11~" 'fixup-whitespace)      ; F1
;; (define-key fnkey-map "12~" 'undo)  ; F2
;; (define-key fnkey-map "13~" 'what-line) ; F3 Tell current line number
;; (define-key fnkey-map "14~" 'goto-line)        ; F4
;; (define-key fnkey-map "15~" 'compile)       ; F5 Run compiler in emacs...
;; (define-key fnkey-map "17~" 'next-error)    ; F6..and move cursor to next
;;                                             ; compilation error.
;; (define-key fnkey-map "18~" 'save-buffer)             ; F7
;; (define-key fnkey-map "19~" 'exchange-point-and-mark) ; F8
;; (define-key fnkey-map "20~" 'kill-region)             ; F9 Kill region
;; (define-key fnkey-map "21~" 'mh-rmail)                      ; F10
;; (define-key fnkey-map "23~" nil)                      ; F11 ESC
;; (define-key fnkey-map "24~" 'adjust-window-up)        ; F12
;; (define-key fnkey-map "25~" 'adjust-window-down)      ; F13
;; ;(define-key fnkey-map "26~" 'large-font)             ; F14
;; (define-key fnkey-map "28~" 'apropos)           ; Help KEY
;; (define-key fnkey-map "29~" 'yank-pop)                ; Do key
;; (define-key fnkey-map "1~" 'isearch-forward)          ; Find key
;; (define-key fnkey-map "2~" 'yank)                     ; Insert Here Key
;; (define-key fnkey-map "4~" 'set-mark-command )        ; Select key
;; ;(define-key fnkey-map "31~" 'small-font)                      ; F17
;; (define-key fnkey-map "32~" 'set-dir-switch-n)        ; F18
;; (define-key fnkey-map "33~" 'set-dir-switch-t)        ; F19
;; (define-key fnkey-map "34~" 'dired)                   ; F20

; A function "nil" does nothing, and acts only as a placeholder for
; convenience in adding new functions later.  The function "beep" does
; the obvious.  (Note: "nil" is a constant and so does not need to be
; quoted; "beep" is the name of a function and needs to be put after
; a single quote (').  All added functions should be quoted in this
;; ; manner.
;; (global-set-key "OD" 'backward-char)
;; (global-set-key "OC" 'forward-char)
;; (global-set-key "OA" 'previous-line)
;; (global-set-key "OB" 'next-line)
;; (global-set-key "m" 'mh-smail)


;; (add-to-list 'load-path "~/dev/config/.emacs.d/el-get/el-get")

;; (unless (require 'el-get nil 'noerror)
;;   (with-current-buffer
;;       (url-retrieve-synchronously
;;        "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
;;     (goto-char (point-max))
;;     (eval-print-last-sexp)))

;; (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
;; (el-get 'sync)


;; (add-to-list 'load-path "~/.emacs.d")
;; (require 'auto-complete-config)
;;(ac-config-default)


;;(add-to-list 'load-path "~/.emacs.d/ac")
;;(require 'auto-complete-config)
;;(ac-config-default)



;;(load-file "~/.emacs.d/themes/color-theme-twilight.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(debug-on-quit nil)
 '(menu-bar-mode nil)
 '(next-line-add-newlines nil)
 '(package-check-signature nil)
 '(package-selected-packages
   (quote
    (highlight-indent-guides evil eval-in-repl undo-tree term-keys goto-chg)))
 '(pop-up-windows nil)
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "grey14" :foreground "grey80" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(mode-line-buffer-id ((t (:background "grey80" :foreground "grey20" :inverse-video t))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey25" :foreground "grey45" :box (:line-width -1 :color "grey40") :weight light)))))

;;; term-keys-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "term-keys" "term-keys.el" (23196 34818 182286
;;;;;;  0))
;;; Generated autoloads from term-keys.el

(autoload 'term-keys/init "term-keys" "\
Set up configured key sequences for the current terminal.

\(fn)" t nil)

(defvar term-keys-mode nil "\
Non-nil if Term-Keys mode is enabled.
See the command `term-keys-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `term-keys-mode'.")

(custom-autoload 'term-keys-mode "term-keys" nil)

(autoload 'term-keys-mode "term-keys" "\
`term-keys' global minor mode.

When enabled, automatically set up configured keys for new frames
on TTY terminals.  If the current frame is on a TTY, set it up as
well.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("term-keys-konsole.el" "term-keys-linux.el"
;;;;;;  "term-keys-pkg.el" "term-keys-st.el" "term-keys-terminal-app.el"
;;;;;;  "term-keys-urxvt.el" "term-keys-xterm.el") (23196 34818 254858
;;;;;;  477000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; term-keys-autoloads.el ends here

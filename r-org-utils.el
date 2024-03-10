;;; r-org-utils.el --- Utilities for working with R, org-mode and ess.
;; Author: Fred Gruber <fgruber@email.com>
;; Created: 2023-07-16
;; Updated: 2023-12-09
;; Version: 0.1
;; Package-Requires: ((emacs "28.0") (ess "18.10") (org "9.5.5"))

;; This package provides utilities to work with R, orgmode, and ess~

;;; Code:

(defvar r-org-utils-jupyter-prefix "jupyter-repl[python 3.11.4]-")
(defvar r-org-utils-show_plot_next_to_r_process nil)
(defvar r-org-utils-python-function-default "print")
;;(defvar r-org-utils-fit-page-docview-only nil)

(defun r-org-utils-plot-region-or-paragraph(&optional arg)
  "execute region or paragraph and save tmp plot to pdf. Then open windows to show pdf"
  (interactive)
    (let*  (
	    (fname (concat (make-temp-file "plot_") ".pdf"))
	    (width (or arg 7))
	  )
      (progn
	(if (use-region-p)
	    (ess-eval-linewise (add-pdf-to-rcode (buffer-substring (region-beginning) (region-end)) fname width width))
	  (progn (ess-eval-linewise (add-pdf-to-rcode (thing-at-point 'paragraph) fname width width)))
	  )
 	;; (with-help-window "*plots*"
	;;   (find-ssfile-at-point)
	;;   )
	;;(sleep-for 0.1)
	(ess-wait-for-process)
	(if r-org-utils-show_plot_next_to_r_process
	    (ess-switch-to-end-of-ESS)
	    )
	(if (window-in-direction 'right)
	    (progn
	      (select-window (window-in-direction 'right))
	      (find-file fname)
	      )
	  (progn
	    (split-window-right)
	    (select-window (window-in-direction 'right))
	    (find-file fname)
	    )
	    )
	;;(split-window-right)
	;;(windmove-right)
	(sleep-for 0.1)
	(if (equal major-mode 'doc-view-mode)
	    (doc-view-fit-page-to-window)
	    )
    )
      )
    )

;;(define-key ess-mode-map (kbd "C-c g") 'r-org-utils-plot-region-or-paragraph)
(define-key ess-mode-map (kbd "C-M-g") 'r-org-utils-plot-region-or-paragraph)


(defun add-pdf-to-rcode(rcomm fname &optional width height)
  "add pdf(tmpfile) and dev.off() to R command"
  (let*  (
	  (newc (concat "pdf('" fname "',width=" (number-to-string width) ",height=" (number-to-string height) ")\n" rcomm  "\n dev.off()"))
	  )
    (eval newc)
      )
  )

;;(add-pdf-to-rcode "asdf" ";" 7 7)

(defun r-org-utils-convert-saveRDS-to-readRDS ()
  "Converts saveRDS string in kill ring readRDS."
  (interactive)
  (let ((orig-str (current-kill 0 t));;(buffer-substring (region-beginning) (region-end))
        (new-str ""))
    (if (string-match "saveRDS(\\(.*\\),.*file=\\(.*[.]RDS.*\\))" orig-str)
        (progn
          (setq new-str (concat "if (!exists(\"" (match-string 1 orig-str) "\"))\n"
                                "  " (match-string 1 orig-str) " = readRDS(" (match-string 2 orig-str) ")"))
          (insert new-str))
      (message "Selected string is not in the correct format."))))


(defun r-org-utils-convert-save-to-load ()
  "Converts selected save string to load format."
  (interactive)
  (let ((orig-str (current-kill 0 t));;(buffer-substring (region-beginning) (region-end))
        (new-str ""))
    (if (string-match "save(\\(.*\\),.*file.*=\\(.*\\))" orig-str)
        (progn
          (setq new-str (concat "if (!exists(\"" (match-string 1 orig-str) "\"))\n"
                                "  load(\"" (match-string 2 orig-str) "\")"))
          (insert new-str))
      (message "String in kill ring is not in the correct format."))))



(defun toggle-true-false ()
  "Toggle the word at point between TRUE and FALSE."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (when bounds
      (let* ((word (buffer-substring-no-properties (car bounds) (cdr bounds)))
             (replacement (if (string= word "TRUE") "FALSE" "TRUE")))
        (delete-region (car bounds) (cdr bounds))
        (insert replacement)))))
(define-key ess-mode-map (kbd "C-c t") 'toggle-true-false)
(define-key org-mode-map (kbd "C-c t") 'toggle-true-false)


(defun toggle-true-false-python ()
  "Toggle the word at point between TRUE and FALSE."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (when bounds
      (let* ((word (buffer-substring-no-properties (car bounds) (cdr bounds)))
             (replacement (if (string= word "True") "False" "True")))
        (delete-region (car bounds) (cdr bounds))
        (insert replacement)))))
(define-key python-mode-map (kbd "C-c t") 'toggle-true-false-python)
(define-key elpy-mode-map (kbd "C-c t") 'toggle-true-false-python)




(defun r-org-utils-copy-last-output-line ()
  (interactive)
  (save-window-excursion
    (ess-switch-to-end-of-ESS)
    (comint-previous-prompt 1)
    (move-end-of-line 1)
    (forward-char 1)
    (push-mark)
    (move-end-of-line 1)
    (kill-ring-save (region-beginning) (region-end))
    )
  (comment-indent)
  (yank)
  )
(define-key ess-mode-map (kbd "C-c l") 'r-org-utils-copy-last-output-line)

(defun r-org-utils-copy-last-output-line-python ()
  (interactive)
  (save-window-excursion
    (elpy-shell-switch-to-shell)
    (comint-previous-prompt 1)
    (move-end-of-line 1)
    (forward-char 1)
    (push-mark)
    (move-end-of-line 1)
    (kill-ring-save (region-beginning) (region-end))
    )
  (comment-indent)
  (yank)
  )
(define-key elpy-mode-map (kbd "C-c l") 'r-org-utils-copy-last-output-line-python)

(defun r-org-utils-elpy-eval-string (fun)
  (interactive)
  (save-window-excursion
    (elpy-shell-switch-to-shell)
    (end-of-buffer)
    (insert fun)
    (comint-send-input)
    )
  )


(defun r-org-utils-add-print (func)
  "enclose line in print(). This is useful in python orgmode blocks"
  (interactive (list (read-string "Function to add: " r-org-utils-python-function-default)))
  (setq r-org-utils-python-function-default func)
  (save-excursion
       (beginning-of-line-text)
       (insert (concat func "("))
       (end-of-line)
       (insert ")")
       )
  )

(define-key python-mode-map (kbd "C-c r") 'r-org-utils-add-print)

(defun r-org-utils-evaluate-r-function (func &optional arg)
  "Add an R-style function call around the word at point to the kill ring, and evaluate it in the default R session. Use C-u to bring back the first line of the output and put it in comment. Use C-u C-u to used %$% instead of %>%"
  (interactive "sEnter R function name: \np")
  (let ((word (thing-at-point 'symbol)))
    (if word
        (let (
	      (text (format "%s %%>%% %s" word func))
	      (text2 (format "%s %%$%% %s" word func))
	      )
	  (if (equal arg 16)
	      (progn
		(message "arg==16")
		(kill-new text2)
		(ess-eval-linewise text2)
	      )
 	    (progn
             (kill-new text)
             (ess-eval-linewise text)
	     )
	    )
	  (if (equal arg 4)
	      (progn
		;;(sleep-for 0.1)
		(ess-wait-for-process)
		(r-org-utils-copy-last-output-line)
		)
	    )
	  )
      (message "No word at point")
      )
    )
  )

(define-key ess-mode-map (kbd "C-c f") 'r-org-utils-evaluate-r-function)

(defun r-org-utils-evaluate-python-function (func &optional arg)
  "Add a function call around the word at point to the kill ring, and evaluate it in the default R session. Use C-u to bring back the first line of the output and put it in comment. Use C-u C-u to used %$% instead of %>%-"
  (interactive "sEnter python function name: \np")
  (let ((word (thing-at-point 'symbol)))
    (if word
        (let (
	      (text2 (format "%s(%s)" func word))
	      (text (format "%s.%s" word func))
	      )
	  (if (equal arg 16)
	      (progn
		;;(message "arg==16")
		;;(kill-new text2)
		(r-org-utils-elpy-eval-string text2)
	      )
 	    (progn
             ;;(kill-new text)
             (r-org-utils-elpy-eval-string text)
	     )
	    )
	  (if (equal arg 4)
	      (progn
		(sleep-for 0.2)
		;;(message "arg==4")
		(r-org-utils-copy-last-output-line-python)
		)
	    )
	  )
      (message "No word at point")
      )
    )
  )

(define-key elpy-mode-map (kbd "C-c f") 'r-org-utils-evaluate-python-function)
(define-key python-mode-map (kbd "C-c f") 'r-org-utils-evaluate-python-function)




(defvar session_name "*R*")

(defun set-ess-R-process()
  "set the process to session stored in session_name variable"
  (when (string= "R" lang)
       (message "Setting R session to %s" session_name)
       (setq ess-local-process-name (process-name (get-buffer-process session_name)))
       )
  (when (string= "python" lang)
    (if (string= session_name "none")
	(setq session_name "Global")
	)
      (message "Setting python session to %s" session_name)
      (r-org-utils-elpy-shell-set-local-shell2 session_name)
      (if (bound-and-true-p ob-reticulate-mode)
	  (progn
	    (message "in reticulate mode activating`' repl")
	   ;; (python-shell-send-string "repl_python()")
	    ;;(r-org-utils-elpy-eval-string "repl_python()")
	    (save-window-excursion
	      (elpy-shell-switch-to-shell)
	      (end-of-buffer)
	      (insert "repl_python()")
	      ;;(comint-send-input)
	      (inferior-ess-send-input)
	      (inferior-python-mode)
	      )
	    ;;(python-shell-send-string python-shell-eval-setup-code)
	    ;;(r-org-utils-elpy-eval-string python-shell-eval-setup-code)
	    ;;(inferior-ess-send-input)

	    ;;(ess-eval-linewise "repl_python()")
	      ;;(r-org-utils-elpy-eval-string "repl_python()")
	    ;; (save-window-excursion
	    ;;   (elpy-shell-switch-to-shell)
	    ;;   (python-remote)
	    ;;   )
	      )
	    )
       ;;(setq python-shell-buffer-name session_name)
      )
    (when (string= "jupyter-python" lang)
      (message "Setting python session to %s" session_name)
      (r-org-utils-elpy-shell-set-local-shell2 session_name)
       ;;(setq python-shell-buffer-name session_name)
      )
  )

(defun get-org-current-rsession()
  "When you are in an org file get the current R session based on  global header, subtree, property or source code :session property whichever is relevant. Save it to variable session_name"
  (interactive)
  (let*
      ((mylist (org-babel-get-src-block-info))
       (prop (nth 2 mylist))
       )
    (progn
      (setq lang (nth 0 mylist))
      (when (string= "R" lang)
	(setq session_name (cdr (assq :session prop)))
	(message "R session name: %s" session_name)
	(set-other-window-func session_name)
	)
      (when (string= "python" lang)
	(setq session_name (cdr (assq :session prop)))
	(message "Setting other window to Python session name: %s" session_name)
	(set-other-window-func session_name)

	;; (if (bound-and-true-p ob-reticulate-mode)
	;;     (progn
	;;       (org-babel-execute:R
	;;        "repl_python()"
	;;        params
	;;        )
	;;       (org-babel-execute:R
	;;        (python-shell-eval-setup-code)
	;;        nil
	;;        )
	;;       )
	;;   )
	)
      (when (string= "jupyter-python" lang)
	(setq session_name (cdr (assq :session prop)))
	(setq session_name (concat r-org-utils-jupyter-prefix session_name))
	(message "Jupyter session name: %s" session_name)
	(set-other-window-func session_name)
	)
      )
    )
  )

(advice-add 'org-edit-special :before #'get-org-current-rsession)
(advice-add 'org-edit-special :after #'set-ess-R-process)

(defun reticulate-cleanup-after()
  "if ob-reticulate is active exit python repl"
  (if (bound-and-true-p ob-reticulate-mode)
      (progn
	(advice-add
	 #'org-babel-execute:python :around #'ob-reticulate-advice)
	)
      )
  )

(defun reticulate-cleanup-before()
  "if ob-reticulate is active exit python repl"
  (if (bound-and-true-p ob-reticulate-mode)
      (progn
	(if (string="python" lang)
	    (progn
	      (message "exiting python repl")
	      (save-window-excursion
	       	(elpy-shell-switch-to-shell)
	       	(inferior-ess-r-mode)
		(setq comint-output-filter-functions '(ansi-color-process-output comint-postoutput-scroll-to-bottom comint-watch-for-password-prompt))
		(insert "exit")
		(comint-send-input)
	       	)
	      ;;(r-org-utils-elpy-eval-string "exit")
	      ;;(python-shell-send-string "exit")

	      )
	  )
	  )
	)
    )

(advice-add 'org-edit-src-exit :before #'reticulate-cleanup-before)
;;(advice-remove 'org-edit-src-exit #'reticulate-cleanup-before)
;;(advice-add 'org-edit-src-exit :after #'reticulate-cleanup-after)
;;(advice-remove 'org-edit-src-exit #'reticulate-cleanup-after)
(defun set-other-window-func(&optional arg)
  "set the other window"
  (unless arg (setq arg nil))
  ;;(message "arg=%s" arg)
  (if arg
      (setq other-window-scroll-buffer
	      (get-buffer arg))
    (progn
      (let*
	  ((window_names (mapcar (lambda (w) (buffer-name (window-buffer w)))
				 (window-list)))
	   (cwindow (list (buffer-name (window-buffer))))
	   (windows_names_filt (cl-set-difference window_names cwindow))
	   )
	(progn
	 (message "windows_names_filt: %s" windows_names_filt)
	 (setq other-window-scroll-buffer
		 (get-buffer (completing-read "Select other window: " windows_names_filt)))
	 )
	)
      )
    )
  )


(defun set-other-window()
  "set other windows to selected window"
  (interactive)
  (set-other-window-func)
  )



(defun automatic-other-window-selection()
  "Return the default behavior of other window"
  (interactive)
  (setq other-window-scroll-buffer nil)
)





(defun r-org-utils-set-other-to-ess-process()
  "set the other window to the R process associated to ess buffer"
  (interactive)
  (let*
      (
       (cprocess ess-local-process-name)
       )
    (progn
      (message "Associated buffer is %s" cprocess)
      (set-other-window-func (process-buffer (get-process cprocess)))
      )
    )
  )

(advice-add 'ess-switch-process :after #'r-org-utils-set-other-to-ess-process)


(defun add-exists-condition-above ()
  (interactive)
  (save-excursion
    (let ((word (thing-at-point 'symbol))) ;; Get the word at point
      (beginning-of-line-text) ;; Move to the beginning of the line
      (open-line 1)
      (insert (format "if(!exists(\"%s\"))" word))
      (forward-line 1) ;; Move to the line above
      (indent-according-to-mode) ;; indent
      )
    )
  )

(defun get-python-inferior-buffers ()
  "Get a list of buffer names with a python inferior session."
  (let (buffer-list)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (eq major-mode 'inferior-python-mode)
          (push (buffer-name buffer) buffer-list))))
    buffer-list))


(defun r-org-utils-elpy-shell-set-local-shell2 (&optional shell-name)
  "Associate the current buffer to a specific shell.
small modification of elpy-shell-set-local-shell function in elpy. Removed prefix in shell name.

Meaning that the code from the current buffer will be sent to this shell.

If SHELL-NAME is not specified, ask with completion for a shell name.

If SHELL-NAME is \"Global\", associate the current buffer to the main python
shell (often \"*Python*\" shell)."
  (interactive)
  (let* ((current-shell-name (if (local-variable-p 'python-shell-buffer-name)
                                 (progn
                                   (string-match "\\(.*?\\)"
                                                 python-shell-buffer-name)
                                   (match-string 1 python-shell-buffer-name))
                               "Global"))
	 (shell-names (get-python-inferior-buffers))
         ;; (shell-names (cl-loop
         ;;        for buffer in (buffer-list)
         ;;        for buffer-name = (substring-no-properties (buffer-name buffer))
         ;;        if (string-match "\\*.*py\\[\\(.*?\\)\\]\\*" buffer-name)
         ;;        collect (match-string 1 buffer-name)))
         (candidates (remove current-shell-name
                           (delete-dups
                            (append (list (buffer-name) "Global")
				    shell-names))))
         (prompt (format "Shell name (current: %s): " current-shell-name))
         (shell-name (or shell-name (completing-read prompt candidates)))
	 (shell-name2 (replace-regexp-in-string "\\*" "" shell-name))
	 )
    (if (string= shell-name2 "Global")
       (kill-local-variable 'python-shell-buffer-name)
      (setq-local python-shell-buffer-name (format "%s" shell-name2)))
    )
  )

(define-key python-mode-map (kbd "C-c C-s") 'r-org-utils-elpy-shell-set-local-shell2)
(define-key elpy-mode-map (kbd "C-c C-s") 'r-org-utils-elpy-shell-set-local-shell2)

(define-key python-mode-map (kbd "C-c s") 'elpy-rgrep-symbol)
(define-key elpy-mode-map (kbd "C-c s") 'elpy-rgrep-symbol)

(defun python-remote()
  (interactive)
  "convert python running on a shell into an python inferior so you can use elpy"
  (end-of-buffer)
  (insert python-shell-eval-setup-code)
  (comint-send-input)
  (comint-send-input)
  (end-of-buffer)
  (insert python-shell-eval-file-setup-code)
  (comint-send-input)
  (comint-send-input)
  (end-of-buffer)
  (inferior-python-mode)
  )


(define-skeleton insertpipe_python
  "insert R pipe operator"
  ""
  " >> "
  )
(define-key python-mode-map (kbd "C-.") 'insertpipe_python)
(define-key elpy-mode-map (kbd "C-.") 'insertpipe_python)
(define-key inferior-python-mode-map (kbd "C-.") 'insertpipe_python)


(define-skeleton insertpipe
  "insert R pipe operator"
  ""
  " %>% "
  )

(define-key ess-mode-map (kbd "C-.") 'insertpipe)
(define-key inferior-ess-mode-map (kbd "C-.") 'insertpipe)
(define-key minibuffer-mode-map (kbd "C-.") 'insertpipe)
;;(global-set-key (kbd "C-.") 'insertpipe)

(define-skeleton insertpipe2
    "insert R pipe operator"
    ""
    " %$% "
    )
(define-key ess-mode-map (kbd "C-,") 'insertpipe2)
(define-key inferior-ess-mode-map (kbd "C-,") 'insertpipe2)






(provide 'r-org-utils)
;;; r-org-utils.el ends here

(defun sniper-mode-aim-response-handler (buffer)
  "Make a handler for evaluating and printing result in BUFFER."
  (nrepl-make-response-handler
   buffer
   (lambda (buffer value)
     (let* ((v (first (read-from-string value)))
            (file (nth 0 v))
            (line (nth 1 v))
            (col (nth 2 v))
            (reason (nth 3 v)))
       (find-file file)
       (goto-line line)
       (move-to-column (- col 1))
       (message (prin1-to-string reason))))
   '()
   (lambda (buffer err)
     (message "%s" err))
   '()))

(defun sniper-mode-command (command)
  (let ((buffer (current-buffer)))
    (nrepl-send-string
     (format "(when-let [a %s] (sniper.scope/aim->el a))" command)
     (sniper-mode-aim-response-handler buffer)
     "user")))

(defun sniper-mode-aim ()
  (interactive)
  (sniper-mode-command "(sniper.scope/aim)"))

(defun sniper-mode-fired () 
  (interactive)
  (save-buffer)
  (kill-buffer)
  (sniper-mode-command "(sniper.scope/fired!)"))

(defun sniper-mode-spare () 
  (interactive)
  (kill-buffer)
  (sniper-mode-command "(sniper.scope/spare!)"))

(define-minor-mode sniper-mode
  "Snipe that dead code"
  :lighter " sniper"
  :global true
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-M-'") 'sniper-mode-aim)
            (define-key map (kbd "<C-M-backspace>") 'sniper-mode-fired)
            (define-key map (kbd "C-M-=") 'sniper-mode-spare)            
            map))

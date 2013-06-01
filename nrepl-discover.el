(require 'nrepl)

;; copied from nrepl-make-response-handler because that's a monolithic ball
(defun nrepl-discover-status (status)
  (when (member "interrupted" status)
    (message "Evaluation interrupted."))
  (when (member "eval-error" status)
    (funcall nrepl-err-handler buffer ex root-ex session))
  (when (member "namespace-not-found" status)
    (message "Namespace not found."))
  (when (member "need-input" status)
    (nrepl-need-input buffer))
  (when (member "done" status)
    (remhash id nrepl-requests)))

(defun nrepl-discover-face (color)
  (let ((face-name (intern (concat "nrepl-discover-" color "-face"))))
    (when (not (symbol-file face-name 'defface))
      (custom-declare-face face-name `((default . (:background ,color)))
                           (concat "Face for nrepl " color " overlays")))
    face-name))

(defun nrepl-discover-overlay (overlay)
  (save-excursion
    (destructuring-bind (color line message) overlay
      (goto-char (point-min))
      (forward-line (1- line))
      (let ((beg (point)))
        (end-of-line)
        (let ((overlay (make-overlay beg (point))))
          (overlay-put overlay 'face (nrepl-discover-face color))
          (when message
            (overlay-put overlay 'message message)))))))

(defun nrepl-discover-op-handler (buffer)
  (lexical-let ((buffer buffer))
    (lambda (response)
      (nrepl-dbind-response response (message ns out err status id ex root-ex
                                              session overlay clear-overlays
                                              text url)
        (when message
          (message message))
        (when text
          (with-current-buffer (format "*nrepl-text*")
            (let ((inhibit-read-only t))
              (delete-region (point-min) (point-max))
              (insert text))
            (setq buffer-read-only t)))
        (when out
          (nrepl-emit-output buffer out t))
        (when err
          (nrepl-emit-output buffer err t))
        (when url
          (browse-url url))
        (when clear-overlays
          (with-current-buffer buffer
            (remove-overlays)))
        (when overlay
          (with-current-buffer buffer
            (nrepl-discover-overlay overlay)))
        (when status
          (nrepl-discover-status status))))))

(defvar nrepl-discover-var nil)

(defun nrepl-discover-choose-var (ns)
  (let ((nrepl-discover-var nil)) ; poor man's promises
    (nrepl-ido-read-var ns (lambda (var) (setq nrepl-discover-var var)))
    ;; async? more like ehsync.
    (while (not nrepl-discover-var)
      (sit-for 0.01))
    (concat nrepl-ido-ns "/" nrepl-discover-var)))

(defun nrepl-discover-argument (arg)
  (list (car arg) (case (intern (cadr arg))
                    ('ns '(if current-prefix-arg
                              (read-from-minibuffer "Namespace: ")
                            (clojure-find-ns)))
                    ('region '(list (point) (mark)))
                    ('var '(nrepl-discover-choose-var (clojure-find-ns)))
                    ('file '(if current-prefix-arg
                                (ido-read-file-name)
                                buffer-file-name))
                    ('position '(format "%s:%s" buffer-file-name (point)))
                    ('list `(completing-read ,(or (nth 2 arg)
                                                  (concat (nth 0 arg) ": "))
                                             ,(nth 3 arg)))
                    (t `(read-from-minibuffer
                         ,(or (nth 2 arg)
                              (concat (nth 0 arg) ": ")))))))

(defun nrepl-discover-command-for (op)
  `(defun ,(intern (concat "nrepl-" (assoc-default "name" op))) ()
     ,(assoc-default "doc" op)
     (interactive)
     (nrepl-send-op ,(assoc-default "name" op)
                    (list ,@(mapcan 'nrepl-discover-argument
                                    (assoc-default "args" op)))
                    (nrepl-discover-op-handler (current-buffer)))))

(defun nrepl-discover ()
  "Query nREPL server for operations and define Emacs commands for them."
  (interactive)
  (nrepl-send-op "discover" ()
                 (nrepl-make-response-handler
                  (current-buffer)
                  (lambda (_ value)
                    (dolist (op value)
                      ;; for some reason the 'dict car needs to be stripped
                      (eval (nrepl-discover-command-for (cdr op)))))
                  nil nil nil nil)))

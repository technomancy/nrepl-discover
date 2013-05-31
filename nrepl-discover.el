(require 'nrepl)

(defun nrepl-discover-op-handler (buffer)
  (lexical-let ((b buffer))
    ;; TODO: support other response types
    (nrepl-make-response-handler b
                                 (lambda (buffer value)
                                   (message value))
                                 nil nil nil)))

(defun nrepl-discover-interactive (arglist)
  ;; TODO: support multiple arguments
  ;; TODO: support completion when argument type is more specific
  (cons 'interactive (if arglist
                         (list (concat "s" (or (nth 2 (car arglist))
                                               (nth 0 (car arglist))))))))

(defun nrepl-discover-command-for (op)
  `(defun ,(intern (concat "nrepl-" (assoc-default "name" op)))
     ,(mapcar 'intern (mapcar 'car (assoc-default "arglist" op)))
     ,(assoc-default "doc" op)
     ,(nrepl-discover-interactive (assoc-default "arglist" op))
     (nrepl-send-op ,(assoc-default "name" op)
                    (list ,@(mapcan (lambda (x) (list (car x) (intern (car x))))
                                    (assoc-default "arglist" op)))
                    (nrepl-discover-op-handler (current-buffer)))))

(defun nrepl-discover ()
  (interactive)
  (nrepl-send-op "discover" ()
                 (nrepl-make-response-handler
                  (current-buffer)
                  (lambda (_ value)
                    (dolist (op value)
                      ;; for some reason the 'dict car needs to be stripped
                      (eval (nrepl-discover-command-for (cdr op)))))
                  nil nil nil nil)))

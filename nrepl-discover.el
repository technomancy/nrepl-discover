;;; nrepl-discover.el --- Client to load commands from nrepl server

;; Copyright © 2013 Phil Hagelberg
;;
;; Author: Phil Hagelberg <technomancy@gmail.com>
;; URL: http://github.com/technomancy/nrepl-discover
;; Version: 0.0.2
;; Keywords: languages, lisp

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This library is a client for Clojure nREPL servers which offer up
;; certain operations as commands which can be invoked client-side.

;; Upon running M-x nrepl-discover, it will query the connected server
;; for operations and use the return value to construct new
;; interactive defuns corresponding to each server-side operation
;; which prompt appropriately for the given types desired.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'cider-client)
(require 'nrepl-client)

(defvar nrepl-discover-var nil)

(defun nrepl-discover-choose-var (ns)
  (let ((nrepl-discover-var nil)) ; poor man's promises
    (cider-ido-read-var (or ns "user")
                        (lambda (var) (setq nrepl-discover-var var)))
    ;; async? more like ehsync.
    (while (not nrepl-discover-var)
      (sit-for 0.01))
    (concat cider-ido-ns "/" nrepl-discover-var)))

(defun nrepl-discover-argument (arg)
  (destructuring-bind (name type prompt) arg
    (list name (case (intern type)
                 ('string `(read-from-minibuffer ,prompt))
                 ;; we already have this implicit in nrepl msgs; needed here?
                 ('ns '(if current-prefix-arg
                           (read-from-minibuffer "Namespace: ")
                         (clojure-find-ns)))
                 ('region '(list buffer-file-name (point) (mark))) ; untested
                 ('var '(nrepl-discover-choose-var (clojure-find-ns)))
                 ('file '(progn
                           (save-some-buffers)
                           (if current-prefix-arg ; untested
                               (ido-read-file-name)
                             buffer-file-name)))
                 ('position '(list buffer-file-name (point))) ; untested
                 ('list `(completing-read ,(or (nth 2 arg) ; untested
                                               (concat (nth 0 arg) ": "))
                                          ,(nth 3 arg)))
                 ;; TODO: eval type
                 (t `(read-from-minibuffer
                      ,(or (nth 2 arg)
                           (concat (nth 0 arg) ": "))))))))

(defun nrepl-discover-command-for (op)
  "Construct a defun command form for `OP'.

Argument should be an alist with \"name\", \"doc\", and \"args\" keys as per
the nrepl-discover docs."
  `(defun ,(intern (concat "nrepl-" (assoc-default "name" op))) ()
     ,(assoc-default "doc" op)
     (interactive)
     (cider-send-op ,(assoc-default "name" op)
                    (list ,@(mapcan 'nrepl-discover-argument
                                    (assoc-default "args" op))))))

(defvar nrepl-discovered-ops nil
  "List of ops discovered by the last `nrepl-discover' run.")

(defun nrepl-discover-make-command (op)
  (when (not (string= "nrepl-discover"
                      (assoc-default "name" (cdr op))))
    (eval (nrepl-discover-command-for (cdr op)))
    (add-to-list 'nrepl-discovered-ops
                 (assoc-default "name" (cdr op)))))

(defun nrepl-discover ()
  "Query nREPL server for operations and define Emacs commands for them."
  (interactive)
  (setq nrepl-discovered-ops nil)
  (cider-send-op "discover" ()
                 (nrepl-make-response-handler
                  (current-buffer)
                  (lambda (_ value)
                    (dolist (op value)
                      (nrepl-discover-make-command op))
                    (message "Loaded nrepl-discover commands: %s."
                             (mapconcat 'identity nrepl-discovered-ops ", ")))
                  nil nil nil nil)))

(provide 'nrepl-discover)
;;; nrepl-discover.el ends here

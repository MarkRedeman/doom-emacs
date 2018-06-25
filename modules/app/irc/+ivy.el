;;; app/irc/ivy.el -*- lexical-binding: t; -*-

;; (defvar ivy-circe-map (make-sparse-keymap))

;; (ivy-set-actions
;;  'ivy-circe
;;  '(("e"
;;     ivy-pass--edit-action
;;     "edit")
;;    ("d"
;;     ivy-pass--delete-action
;;     "delete")
;;    ("a"
;;     ivy-pass--add-action
;;     "add")
;;    ("r"
;;     ivy-pass--rename-action
;;     "rename")
;;    ("g"
;;     ivy-pass--generate-action
;;     "generate")))

(message "Loading ivy irc")

(defun ivy-circe/circe-channel-buffers ()
  "Filter for circe channel buffers."
  (cl-loop for buf in (buffer-list)
           if (eq 'circe-channel-mode (buffer-local-value 'major-mode buf))
           collect (buffer-name buf)))

(defun ivy-circe/circe-server-buffers ()
  "Filter for circe server buffers."
  (cl-loop for buf in (buffer-list)
           if (eq 'circe-server-mode (buffer-local-value 'major-mode buf))
           collect (buffer-name buf)))

(defun ivy-circe/circe-query-buffers ()
  "Filter for circe query buffers."
  (cl-loop for buf in (buffer-list)
           if (eq 'circe-query-mode (buffer-local-value 'major-mode buf))
           collect (buffer-name buf)))

(defun ivy-circe/circe-recent-buffers ()
  "Filter for circe buffers with activity."
  tracking-buffers)

;;; ivy-circe.el ends here

;; https://sam217pa.github.io/2016/09/13/from-helm-to-ivy/

;; (ivy-read
;;  "Channel:"
;;  (ivy-circe/circe-channel-buffers)
;;  :keymap ivy-circe-map)


;; Use: M-o to select an action (i.e. goto, part, join ?)
;; (my-command-with-3-actions)

;; TODO: Make this smart, that is use autocompletion for who
(defun +circe/circe-send-message (who what)
  "Send WHO a message containing WHAT."
  (interactive "sWho: \nsWhat: ")
  (circe-command-MSG who what))

(defun ivy-circe/switch-to-buffer (buffer)
  (if (+workspace-exists-p +irc--workspace-name)
      (+workspace-switch +irc--workspace-name))
  (switch-to-buffer buffer))

(defun ivy-circe/read (name list)
  (ivy-read name list :action
            '(1 ("o" ivy-circe/switch-to-buffer "Open")
                ("k" (lambda (candidate) (bury-buffer candidate)) "Kill"))))

;; Use M-o
;;;###autoload
(defun ivy-circe ()
  "Select circe channel, server and query buffers using ivy."
  (interactive)
  (let ((channels (ivy-circe/circe-channel-buffers))
        (queries (ivy-circe/circe-query-buffers))
        (servers (ivy-circe/circe-server-buffers)))
    (ivy-circe/read "Channel:" (append channels queries servers))))


;;;###autoload
(defun ivy-circe/circe-new-activity ()
  "Get a list of all active channels or queries."
  (interactive)
  (ivy-circe/read "Active:" (ivy-circe/circe-recent-buffers)))

;;;###autoload
(defun ivy-circe/channels ()
  "Displays a candidate list consisting of all channels from every server."
  (interactive)
  (ivy-read "Channels:" (ivy-circe/circe-channel-buffers)))

;;;###autoload
(defun ivy-circe/servers ()
  "Displays a candidate list consiting of all servers."
  (interactive)
  (ivy-read "Servers:" (ivy-circe/circe-server-buffers)))

;;;###autoload
(defun ivy-circe/queries ()
  "Displays a candidate list consistin of all queries."
  (interactive)
  (ivy-read "Queries:" (ivy-circe/circe-query-buffers)))


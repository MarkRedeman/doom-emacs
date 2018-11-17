;; -*- no-byte-compile: t; -*-
;;; app/irc/packages.el

(package! circe)
(package! circe-notifications)
(when (featurep! :completion helm)
  (package! helm-circe))

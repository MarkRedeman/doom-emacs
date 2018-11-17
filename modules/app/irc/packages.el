;; -*- no-byte-compile: t; -*-
;;; app/irc/packages.el

(package! circe :pin "0c79138fb2")
(package! circe-notifications :pin "291149ac12")
(when (featurep! :completion helm)
  (package! helm-circe))

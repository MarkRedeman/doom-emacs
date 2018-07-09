;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

;; requires: python jedi setuptools

(package! nose)
(package! pip-requirements)
(cond ((featurep! :tools +lsp)
        (featurep! +lsp))
      ((featurep! :completion company)
        (package! company-anaconda)))
(when (featurep! +conda)
  (package! conda))

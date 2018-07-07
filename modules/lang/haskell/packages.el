;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode)
(when (featurep! :completion company)
  (package! company-ghc))

;;
(cond ((featurep! +lsp) (depends-on! :tools lsp (package! lsp-haskell)))
      ((featurep! +dante)
       (package! dante))
      (t
       (package! intero)
       (package! hindent)))

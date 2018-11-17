;; -*- no-byte-compile: t; -*-
;;; lang/julia/packages.el

(package! julia-mode)
(package! julia-repl)
;; (package! flycheck-julia)
(package! lsp-julia :recipe (:fetcher github :repo "non-Jedi/lsp-julia"))

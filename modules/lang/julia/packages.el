;; -*- no-byte-compile: t; -*-
;;; lang/julia/packages.el

(package! julia-mode :pin "5238f9adb7")
(package! julia-repl :pin "b11a572970")
;; (package! flycheck-julia)
(package! lsp-julia :recipe (:host github :repo "non-Jedi/lsp-julia"))

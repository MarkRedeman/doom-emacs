;; -*- no-byte-compile: t; -*-
;;; lang/julia/packages.el

(package! julia-mode :pin "5238f9adb7")
(package! julia-repl :pin "b11a572970")
;; (package! julia-mode :recipe (:host github :repo "non-Jedi/julia-emacs" :branch "julia-mode-latexsubs_feature"))
;; (package! flycheck-julia)
(package! lsp-julia)

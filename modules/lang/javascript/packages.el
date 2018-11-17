;; -*- no-byte-compile: t; -*-
;;; lang/javascript/packages.el

;; major modes
(package! coffee-mode)
(package! js2-mode)
(package! rjsx-mode)
(package! typescript-mode)

;; tools
(package! eslintd-fix)
(package! js2-refactor)
(package! nodejs-repl)
(package! npm-mode)
(package! skewer-mode)
(package! jest)
(package! vue-mode)
;; mocha.el? https://github.com/scottaj/mocha.el
;; jest-coverage https://github.com/jcubic/jest-coverage.el

(when (featurep! :feature lookup)
  (package! xref-js2))

(unless (featurep! +lsp)
  (package! tide))

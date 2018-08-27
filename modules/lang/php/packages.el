;; -*- no-byte-compile: t; -*-
;;; lang/php/packages.el

(package! php-boris)
(package! php-extras :recipe (:fetcher github :repo "arnested/php-extras"))
(package! php-mode)
(package! composer)
(package! php-refactor-mode)
(package! php-cs-fixer)
(package! phpactor :recipe (:fetcher github :repo "emacs-php/phpactor.el" :files ("*.el")))
(when (featurep! :feature syntax-checker)
  (package! flycheck-phpstan))
(package! phpunit)

(when (featurep! +hack)
  (package! hack-mode :recipe (:fetcher github :repo "hhvm/hack-mode")))

(unless (featurep! +lsp)
  (package! phpactor :recipe (:fetcher github :repo "emacs-php/phpactor.el" :files ("*"))))

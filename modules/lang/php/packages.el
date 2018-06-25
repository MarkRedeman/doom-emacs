;; -*- no-byte-compile: t; -*-
;;; lang/php/packages.el

(package! php-boris)
(package! php-extras :recipe (:fetcher github :repo "arnested/php-extras"))
(package! php-mode)
(package! php-refactor-mode)
(package! phpunit)

;; (when (featurep! :completion company)
;;   (package! company-php))

;; (package! hack-mode
;;   :recipe
;;   (:fetcher url :url "https://raw.githubusercontent.com/facebook/hhvm/master/hphp/hack/editor-plugins/emacs/hack-mode.el"))

;; !!!!!!!!!!!!!!!!!!
(cond
 ((featurep! +lsp)
  (depends-on! :tools lsp)
  (package! lsp-php))
 ((featurep! :completion company)
  (package! company-php)))

;; (when (featurep! +lsp)
;;   (depends-on! :tools lsp)
;;   (package! lsp-php))

;; (add-hook 'php-mode-hook #'lsp-php-enable)

;; +(use-package lsp-php
;; +  :ensure t
;; +  :after (php-mode lsp-mode)
;; +  :config
;; +  (add-hook 'php-mode-hook #'lsp-php-enable))
;; +

;; https://github.com/Fuco1/.emacs.d/commit/908ea628db826c74dd335543d75254395b0aeb4f
;; https://github.com/NateEag/.emacs.d/commit/ee00ea71c6af10da9492172d3378fec4edb5600c

;; Do stuff with lookup?

;; https://github.com/MarTango/.emacs.d/blob/master/init.el
;; geben
;; php
;; https://github.com/Fuco1/php-refactor




;; (defun my/php-mode-hook ()
;;   "Gets run on php-mode load."
;;   (php-eldoc-enable)
;;   (make-local-variable 'company-backends)
;;   (add-to-list 'company-backends '(company-phpactor php-extras-company company-dabbrev-code))
;;   (flycheck-select-checker 'phpstan)
;;   (setq php-mode-coding-style 'psr2
;;         c-basic-offset 4)
;;   (when (eq 0 (buffer-size))
;;     (insert "<?php\n\n")))

;; (use-package php-mode :ensure t
;;   :init (add-hook 'php-mode-hook #'my/php-mode-hook))
;; (use-package php-extras :defer t :ensure t :after php-mode)
;; (use-package php-auto-yasnippets :defer t :ensure t :after php-mode
;;   :bind (:map php-mode-map ("C-c C-y" . yas/create-php-snippet)))
;; (use-package php-eldoc :ensure t :after php-mode)
;; (use-package flycheck-phpstan :ensure t :after (php-mode flycheck))
;; ;; (use-package ggtags :defer t :ensure t :init (add-hook 'php-mode-hook #'ggtags-mode))
;; (use-package psysh :ensure t :after php-mode
;;   :config
;;   (evil-define-key '(normal insert) php-mode-map
;;     (kbd "C-c C-z") 'psysh)
;;   )

;; (use-package php-refactor-mode :load-path "site-lisp/" :defer t
;;   :commands php-refactor-mode :init (add-hook 'php-mode-hook #'php-refactor-mode))
;; (use-package company-phpactor :load-path "site-lisp/phpactor.el")
;; (use-package phpactor :load-path "site-lisp/phpactor.el"
;;   :after (evil php-mode)
;;   :config
;;   (evil-define-key 'normal php-mode-map
;;     "gd" #'phpactor-goto-definition
;;     (kbd "<S-tab>") #'company-phpactor))
;; (use-package flycheck-phanclient :disabled :load-path "site-lisp/flycheck-phanclient")

;; (use-package phpunit :ensure t)
;; (use-package phpcbf :ensure t :config (setq phpcbf-standard "PSR2"))
;; (use-package phan :defer t)
;; (use-package fluca-php :load-path "site-lisp/")

;; (defun geben-php-run ()
;;   "Start the geben listener, then run the current script with xdebug configuration to point to geben listener."
;;   (interactive)
;;   (call-interactively #'geben)
;;   (let ((cmd (list "php" "-d"
;;                    "xdebug.remote_enable=on" "-d"
;;                    "xdebug.remote_host=127.0.0.1" "-d"
;;                    "xdebug.remote_port=9000" "-d"
;;                    "xdebug.remote_handler=dbgp" "-d"
;;                    "xdebug.idekey=geben" "-d"
;;                    "xdebug.remote_autostart=On"
;;                    (buffer-file-name))))
;;     (apply #'start-process  "GebenPHPDebug" "*geben*" cmd)))

;; (use-package geben
;;   :ensure t
;;   :after evil
;;   :init
;;   (add-hook 'geben-mode-hook #'evil-emacs-state)
;;   :config
;;   (evil-define-key 'normal php-mode-map
;;     (kbd "C-c C-d") #'geben-php-run))



(cond
 ((featurep! :completion helm)
  (package! helm-phpunit))
 ((featurep! :completion ivy)
  (package! ivy-phpunit)))

(package! flycheck-phpstan)

(package! phpactor)
(package! company-phpactor
  :recipe (:fetcher github
                    :repo "emacs-php/phpactor.el"
                    :files ("company-phpactor.el")))

(package! php-cs-fixer)

;; https://github.com/TysonAndre/flycheck-phanclient
;; https://github.com/TysonAndre/lsp-phan
;; (package! flycheck-phanclient :disabled :load-path "site-lisp/flycheck-phanclient")

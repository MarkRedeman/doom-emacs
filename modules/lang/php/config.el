;;; lang/php/config.el -*- lexical-binding: t; -*-

(defvar +php--company-backends nil)

;; (def-package! hack-mode
;;   :mode "\\.hh$"
;;   :config
;;   (set-company-backend! 'hack-mode '(company-capf)))


(def-package! php-mode
  :mode "\\.inc\\'"
  :interpreter "php"
  :config
  (add-hook! php-mode 'flycheck-mode)
  (add-hook 'php-mode-hook 'php-enable-psr2-coding-style)

  ;; Disable HTML compatibility in php-mode. `web-mode' has superior support for
  ;; php+html. Use the .phtml
  (setq php-template-compatibility nil)

  (set-repl-handler! 'php-mode #'php-boris)
  (set-lookup-handlers! 'php-mode :documentation #'php-search-documentation)

  ;; ac-php provides custom autocompletion, php-extras provides autocompletion
  ;; for built-in libraries
  (when +php--company-backends
    (set-company-backend! 'php-mode +php--company-backends))

  ;; Use the smallest `sp-max-pair-length' for optimum `smartparens' performance
  (setq-hook! 'php-mode-hook sp-max-pair-length 6)

  (sp-with-modes '(php-mode)
    (sp-local-pair "<?"    "?>" :post-handlers '(("| " "SPC" "=") ("||\n[i]" "RET") ("[d2]" "p")))
    (sp-local-pair "<?php" "?>" :post-handlers '(("| " "SPC") ("||\n[i]" "RET"))))

  (map! :map php-mode-map
        :localleader
        (:prefix "r"
          :n "cv" #'php-refactor--convert-local-to-instance-variable
          :n "u"  #'php-refactor--optimize-use
          :v "xm" #'php-refactor--extract-method
          :n "rv" #'php-refactor--rename-local-variable)
        (:prefix "t"
          :n "r"  #'phpunit-current-project
          :n "a"  #'phpunit-current-class
          :n "s"  #'phpunit-current-test)))

;; !!!!!!!!!!!!!!!!!!
;; (if (featurep! +lsp) (require! :tools lsp))
;; (def-package! lsp-typescript
;;   :when (featurep! +lsp)
;;   :hook ((js2-mode typescript-mode) . lsp-typescript-enable))

;; https://www.reddit.com/r/emacs/comments/7xz6bd/im_having_the_worst_time_trying_to_get_php/
(def-package! lsp-php
  :when (and (featurep! +lsp) (featurep! :tools lsp))
  :hook (php-mode . lsp-php-enable))

(def-package! php-refactor-mode
  :hook php-mode)


(def-package! php-extras
  :after php-mode
  :init
  ;; company will set up itself
  (advice-add #'php-extras-company-setup :override #'ignore)
  (add-to-list '+php--company-backends 'php-extras-company nil #'eq)
  :config
  (setq php-extras-eldoc-functions-file
        (concat doom-etc-dir "php-extras-eldoc-functions"))

  ;; Make expensive php-extras generation async
  (unless (file-exists-p (concat php-extras-eldoc-functions-file ".el"))
    (message "Generating PHP eldoc files...")
    (require 'async)
    (async-start (lambda ()
                   ,(async-inject-variables "\\`\\(load-path\\|php-extras-eldoc-functions-file\\)$")
                   (require 'php-extras-gen-eldoc)
                   (php-extras-generate-eldoc-1 t))
                 (lambda (_)
                   (load (concat php-extras-eldoc-functions-file ".el"))
                   (message "PHP eldoc updated!")))))


(def-package! company-php
  :when (featurep! :completion company)
  :commands (ac-php-remake-tags ac-php-remake-tags-all)
  :hook (php-mode . ac-php-core-eldoc-setup)
  :init
  (add-to-list '+php--company-backends 'company-ac-php-backend nil #'eq)
  :config (setq ac-php-tags-path (concat doom-cache-dir "ac-php/")))

  ;; (add-hook 'php-mode-hook #'ac-php-core-eldoc-setup)

  ;; ac-php provides custom autocompletion, php-extras provides autocompletion
  ;; for built-in libraries

  ;; (set-company-backend! 'php-mode '(company-ac-php-backend php-extras-company))


;;
;; Projects
;;

(def-project-mode! +php-laravel-mode
  :modes (php-mode yaml-mode web-mode nxml-mode js2-mode scss-mode)
  :files (and "artisan" "server.php"))

(def-project-mode! +php-composer-mode
  :modes (web-mode php-mode)
  :files ("composer.json"))


(def-package! ivy-phpunit
  :after php-mode
  :commands (ivy-phpunit-test-function ivy-phpunit-test-class ivy-phpunit-list-test-classes)
  :config
  (map! :map php-mode-map
        :localleader
        :prefix "t"
        (:prefix "i"
          :n "t"  #'ivy-phpunit-test-function
          :n "c"  #'ivy-phpunit-test-class
          :n "C"  #'ivy-phpunit-list-test-classes)))

(def-package! helm-phpunit
  :commands (helm-phpunit-select-test))

(def-package! flycheck-phpstan
  :after php-mode
  :config
  (defun flycheck-phpstan-setup ()
    "Setup Flycheck Phpstan.
Add `phpstan' to `flycheck-checkers'."
    (interactive)
    (add-to-list 'flycheck-checkers 'phpstan))

  (add-hook 'flycheck-mode-hook #'flycheck-phpstan-setup)
  (add-hook 'php-mode-hook
            (lambda () (flycheck-select-checker 'phpstan))))

(def-package! company-phpactor
  :when (featurep! :completion company)
  :commands (company-phpactor)
  :config
  (set-company-backend! 'php-mode '(company-phpactor)))

(def-package! phpactor
  :after php-mode
  :config
  (map! :map php-mode-map :localleader :prefix "rp" :desc "phpactor"
        :n "g" #'phpactor-goto-definition
        :n "c" #'company-phpactor))

(def-package! php-cs-fixer
          :commands (php-cs-fixer-fix)
          :config
          (setq-default php-cs-fixer-rules-level-part-options "@PSR2"))

;;; lang/php/config.el -*- lexical-binding: t; -*-

(defvar +php--company-backends nil
  "List of company backends to use in `php-mode'.")

(after! projectile
  (add-to-list 'projectile-project-root-files "composer.json"))


;;
;;; Packages

(use-package! php-mode
  :mode "\\.inc\\'"
  :config
  ;; Disable HTML compatibility in php-mode. `web-mode' has superior support for
  ;; php+html. Use the .phtml
  (setq php-mode-template-compatibility nil)

  (set-docsets! 'php-mode "PHP" "PHPUnit" "Laravel" "CakePHP" "CodeIgniter" "Doctrine_ORM")
  (set-repl-handler! 'php-mode #'php-boris)
  ;; (set-repl-handler! 'php-mode #'psysh)
  (set-lookup-handlers! 'php-mode :documentation #'php-search-documentation)

  (if (featurep! +lsp)
      (add-hook 'php-mode-local-vars-hook #'lsp!)
    ;; `+php-company-backend' uses `company-phpactor', `php-extras-company' or
    ;; `company-dabbrev-code', in that order.
    (when +php--company-backends
      (set-company-backend! 'php-mode
        (cons :separate +php--company-backends)
        'company-dabbrev-code)))

  ;; Use the smallest `sp-max-pair-length' for optimum `smartparens' performance
  (setq-hook! 'php-mode-hook sp-max-pair-length 5)

  (sp-with-modes '(php-mode)
    (sp-local-pair "<?"    "?>" :post-handlers '(("| " "SPC" "=") ("||\n[i]" "RET") ("[d2]" "p")))
    (sp-local-pair "<?php" "?>" :post-handlers '(("| " "SPC") ("||\n[i]" "RET"))))

  (map! :localleader
        :map php-mode-map
        :prefix ("t" . "test")
        "r"  #'phpunit-current-project
        "a"  #'phpunit-current-class
        "s"  #'phpunit-current-test
        (:prefix "t"
          "e"  #'+phpunit-toggle-stop-on-error
          "f"  #'+phpunit-toggle-stop-on-failure
          "s"  #'+phpunit-toggle-stop-on-skip))

  (map! :map +php-composer-mode-map
        :localleader
        :prefix "c"
        :desc "Composer"
        (:desc "Run composer"                "C"  #'composer
         :desc "Run vendor/bin command"      "c"  #'composer-run-vendor-bin-command
         :desc "Run script"                  "s"  #'composer-run-script
         :desc "Require and install package" "i"  #'composer-install
         :desc "Install package"             "r"  #'composer-require
         :desc "Update packages"             "u"  #'composer-update
         :desc "Open composer.json"          "o"  #'composer-find-json-file
         :desc "Dumpautoload"                "d"  #'composer-dump-autoload)))

(def-package! flycheck-phpstan
  :when (featurep! :feature syntax-checker)
  :after php-mode)

(def-package! flycheck-phanclient
  :when (featurep! :feature syntax-checker)
  :after php-mode)

(def-package! phpactor
  :after php-mode
  :init
  (add-to-list '+php--company-backends #'company-phpactor nil 'eq)
  :config
  (set-lookup-handlers! 'php-mode
    :definition #'phpactor-goto-definition)

  (map! :localleader
        :map php-mode-map
        :prefix "n"
        ;; Class references
        ;; Class Member References
        ;; Jump to definition
        ;; Jump to (or generate) related file
        "n"  #'phpactor-navigate
        :prefix ("r" . "refactor")
        "C"  #'phpactor-context-menu
        "cc" #'phpactor-copy-class
        "cm" #'phpactor-move-class
        "oi" #'phpactor-offset-info
        "t"  #'phpactor-transform
        "ci" #'phpactor-import-class))


(def-package! company-phpactor :after php-mode :unless (featurep! +lsp))

(use-package! php-extras
  :after php-mode
  :preface
  ;; We'll set up company support ourselves
  (advice-add #'php-extras-company-setup :override #'ignore)
  :init
  (add-to-list '+php--company-backends #'php-extras-company)
  :config
  (setq php-extras-eldoc-functions-file
        (concat doom-etc-dir "php-extras-eldoc-functions"))
  ;; Silence warning if `php-extras-eldoc-functions-file' hasn't finished
  ;; generating yet.
  (defun php-extras-load-eldoc ()
    (require 'php-extras-eldoc-functions php-extras-eldoc-functions-file t))
  ;; Make expensive php-extras generation async
  (unless (file-exists-p (concat php-extras-eldoc-functions-file ".el"))
    (message "Generating PHP eldoc files...")
    (require 'async)
    (async-start `(lambda ()
                    ,(async-inject-variables "\\`\\(load-path\\|php-extras-eldoc-functions-file\\)$")
                    (require 'php-extras-gen-eldoc)
                    (php-extras-generate-eldoc-1 t))
                 (lambda (_)
                   (load (concat php-extras-eldoc-functions-file ".el"))
                   (message "PHP eldoc updated!")))))


(use-package! hack-mode
  :when (featurep! +hack)
  :mode "\\.hh$")

(def-package! php-cs-fixer
  :after php-mode
  :commands (php-cs-fixer-fix php-cs-fixer-before-save)
  :init
  (add-hook! 'php-mode-hook
    (add-hook 'before-save-hook #'php-cs-fixer-before-save nil t))

  (set-formatter! 'php-mode #'php-cs-fixer-fix)


  ;; TODO
  ;; use advice to rewrite build-rules-options so that the
  ;; php-cs-fixer-config-option is set based on current projectile settings
  ;; that is, try to find a .php_cs.dist file or simliar in the current project.
  ;; If no file can be found look for a file in the current dir and upward.
  ;; If still no file can be found, try looking for it in .doom.d, otherwise
  ;; don't change it and assume the fixer settings have been configured
  ;; php-cs-fixer--build-rules-options
  ;; phpstan does something similar so we should check that...

  (map! :map php-mode-map
        :localleader
        :desc "Fix formatting" "f" #'php-cs-fixer-fix))

;;
;; Projects

(def-project-mode! +php-laravel-mode
  :modes '(php-mode yaml-mode web-mode nxml-mode js2-mode scss-mode)
  :files (and "artisan" "server.php"))

(def-project-mode! +php-composer-mode
  :modes '(web-mode php-mode)
  :files ("composer.json"))

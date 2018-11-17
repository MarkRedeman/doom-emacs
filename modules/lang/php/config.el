;;; lang/php/config.el -*- lexical-binding: t; -*-

(def-package! php-mode
  :mode "\\.inc\\'"
  :config
  ;; Disable HTML compatibility in php-mode. `web-mode' has superior support for
  ;; php+html. Use the .phtml
  (setq php-template-compatibility nil)

  (set-docsets! 'php-mode "PHP" "PHPUnit" "Laravel" "CakePHP" "CodeIgniter" "Doctrine_ORM")
  (set-repl-handler! 'php-mode #'php-boris)
  (set-lookup-handlers! 'php-mode :documentation #'php-search-documentation)

  (if (featurep! +lsp)
      (add-hook 'php-mode-hook #'lsp!)
    ;; `+php-company-backend' uses `company-phpactor', `php-extras-company' or
    ;; `company-dabbrev-code', in that order.
    (set-company-backend! 'php-mode '+php-company-backend 'company-dabbrev-code))

  ;; Use the smallest `sp-max-pair-length' for optimum `smartparens' performance
  (setq-hook! 'php-mode-hook sp-max-pair-length 5)

  (sp-with-modes '(php-mode)
    (sp-local-pair "<?"    "?>" :post-handlers '(("| " "SPC" "=") ("||\n[i]" "RET") ("[d2]" "p")))
    (sp-local-pair "<?php" "?>" :post-handlers '(("| " "SPC") ("||\n[i]" "RET"))))

  (map! :map php-mode-map
        :localleader
        :prefix "t"
        "r"  #'phpunit-current-project
        "a"  #'phpunit-current-class
        "s"  #'phpunit-current-test
        (:prefix "t"
          "e"  #'+phpunit-toggle-stop-on-error
          "f"  #'+phpunit-toggle-stop-on-failure
          "s"  #'+phpunit-toggle-stop-on-skip))

  (map! :map php-mode-map
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
  :unless (featurep! +lsp)
  :after php-mode
  :config
  (set-lookup-handlers! 'php-mode
    :definition #'phpactor-goto-definition)

  ;; TODO PR these for phpactor.el?
  ;; company-phpactor breaks company if executable doesn't exist
  (defun +php*company-phpactor-fail-silently (orig-fn &rest args)
    (when (phpactor-find-executable)
      (apply orig-fn args)))
  (advice-add #'company-phpactor :around #'+php*company-phpactor-fail-silently)

  ;; `phpactor-get-working-dir' throws stringp errors if not in a project.
  (defun +php*project-root (&rest _)
    (setq phpactor-working-dir
          (or phpactor-working-dir
              (php-project-get-root-dir)
              (doom-project-root)
              default-directory)))
  (advice-add #'phpactor-get-working-dir :before #'+php*project-root)

  (map! :localleader
        :map php-mode-map
        :prefix "r"
        "cc" #'phpactor-copy-class
        "mc" #'phpactor-move-class
        "oi" #'phpactor-offset-info
        "t"  #'phpactor-transform
        "ic" #'phpactor-import-class))

(def-package! company-phpactor :after php-mode)

(def-package! php-refactor-mode
  :hook php-mode
  :config
  (map! :localleader
        :map php-refactor-mode-map
        :prefix "r"
        "cv" #'php-refactor--convert-local-to-instance-variable
        "u"  #'php-refactor--optimize-use
        "xm" #'php-refactor--extract-method
        "rv" #'php-refactor--rename-local-variable))


(def-package! php-extras
  :after php-mode
  :preface
  ;; We'll set up company support ourselves
  (advice-add #'php-extras-company-setup :override #'ignore)
  :config
  (setq php-extras-eldoc-functions-file
        (concat doom-etc-dir "php-extras-eldoc-functions"))
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


(def-package! hack-mode
  :when (featurep! +hack)
  :mode "\\.hh$")

(def-package! php-cs-fixer
  :after php-mode
  :commands (php-cs-fixer-fix php-cs-fixer-before-save)
  :init
  (add-hook! 'php-mode-hook
    (add-hook 'before-save-hook #'php-cs-fixer-before-save nil t))

  (set-formatter! 'php-mode #'php-cs-fixer-fix)

  (map! :map php-mode-map
        :localleader
        :desc "Fix formatting" "f" #'php-cs-fixer-fix))

;;
;; Projects

(def-project-mode! +php-laravel-mode
  :modes (php-mode yaml-mode web-mode nxml-mode js2-mode scss-mode)
  :files (and "artisan" "server.php"))

(def-project-mode! +php-composer-mode
  :modes (web-mode php-mode)
  :files ("composer.json"))

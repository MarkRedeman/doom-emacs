;;; tools/lsp/config.el -*- lexical-binding: t; -*-

;; TODO look into https://github.com/syl20bnr/spacemacs/pull/10486/files

(def-package! lsp-mode
  :commands (lsp-mode lsp-define-stdio-client))

(def-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (set! :lookup 'lsp-ui-mode
    :definition #'lsp-ui-peek-find-definitions
    :references #'lsp-ui-peek-find-references)
  (setq lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 35
        lsp-ui-sideline-ignore-duplicate t))

(def-package! company-lsp
  :after lsp-mode
  :config
  (message "company-lsp!")
  (set! :company-backend 'lsp-mode '(company-lsp))
  (setq company-lsp-enable-recompletion t))



;; (use-package lsp-mode
;; :ensure t
;; :commands lsp-mode
;; :config (setq lsp-response-timeout 4
;;               lsp-enable-eldoc t
;;               lsp-enable-completion-at-point t))

;; (use-package lsp-ui
;; :ensure t
;; :after lsp-mode
;; :hook (lsp-mode . lsp-ui-mode)
;; :config (setq lsp-ui-flycheck-enable t))

;; (use-package php-mode
;; :ensure t
;; :config (lsp-mode t))


;; (use-package lsp-php
;; :ensure t
;; :after (php-mode lsp-mode)
;; :hook ((php-mode . lsp-php-enable))
;; :custom (lsp-php-server-install-dir (expand-file-name "~/.composer/")))

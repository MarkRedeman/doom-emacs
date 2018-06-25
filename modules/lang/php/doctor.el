;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/php/doctor.el


;; TODO check if executables exists
;; - phpstan
;; - phpactor
;; - langaugeserver
;; - phpunit
;; - php cs


;; (when (featurep! +dante)
;;   (unless (executable-find "cabal")
;;     (warn! "Couldn't find cabal, haskell-mode may have issues"))

;;   (unless (executable-find "ghc-mod")
;;     (warn! "Couldn't find ghc-mod on PATH. Code completion will not work")))

;; (when (featurep! +intero)
;;   (unless (executable-find "stack")
;;     (warn! "Couldn't find stack. Intero will not work")))

;;;
;; (when (featurep! +lsp)
;;   (unless (executable-find "php-language-server.php")
;;     (warn! "Couldnt find the Haskell IDE Engine. LSP support will not work.")))


;; - cmd: cd src/Extern/Companies/
;;   when: 1529853136
;;   paths:
;;     - src/Extern/Companies/
;; - cmd: phpactor
;;   when: 1529853138
;; - cmd: phpactor class:new ContactPerson
;;   when: 1529853151
;; - cmd: phpactor class:new Francken\\Extern\\Companies\\ContactPerson
;;   when: 1529853167
;; - cmd: z P
;;   when: 1529853249
;; - cmd: phpactor class:transform src/Extern/Companies/Company.php
;;   when: 1529853278
;;   paths:
;;     - src/Extern/Companies/Company.php
;; - cmd: phpactor class:transform src/Extern/Companies/Company.php --transform=add_missing_assignments
;;   when: 1529853288
;;   paths:
;;     - src/Extern/Companies/Company.php
;; - cmd: phpactor class:transform src/Extern/Companies/Company.php --transform=  (add-hook 'php-mode-hook 'php-enable-psr2-coding-style)\n
;;   when: 1529853303
;;   paths:
;;     - src/Extern/Companies/Company.php
;; - cmd: phpactor class:transform src/Extern/Companies/Company.php --transform=add_missing_assignments
;;   when: 1529853320
;;   paths:
;;     - src/Extern/Companies/Company.php
;; - cmd: phpactor class:transform src/Extern/Companies/Company.php --transform=add_missing_properties
;;   when: 1529853327
;;   paths:
;;     - src/Extern/Companies/Company.php
;; - cmd: phpactor class:transform src/Extern/Companies/Company.php --transform=complete_constructor
;;   when: 1529853345
;;   paths:
;;     - src/Extern/Companies/Company.php
;; - cmd: phpactor class:transform src/Extern/Companies/Company.php --transform=implemnet_contracts
;;   when: 1529853354
;;   paths:
;;     - src/Extern/Companies/Company.php
;; - cmd: phpactor class:transform src/Extern/Companies/Company.php --transform=implement_contracts
;;   when: 1529853409
;;   paths:
;;     - src/Extern/Companies/Company.php
;; - cmd: phpactor class:transform src/Extern/Companies/Company.php --transform=fix_namespace_class_name
;;   when: 1529853419
;;   paths:
;;     - src/Extern/Companies/Company.php
;; - cmd: phpactor src/Extern/Companies/ContactPerson.php
;;   when: 1529853566
;; - cmd: phpactor class:new src/Extern/Companies/ContactPerson.php
;;   when: 1529853572

;;; lang/php/autoload.el -*- lexical-binding: t; -*-

(defvar +php-composer-conf (make-hash-table :test 'equal))

;;;###autoload
(defun +php-composer-conf (&optional project-root refresh-p)
  "Retrieve the contents of composer.json as an alist. If REFRESH-P is non-nil
ignore the cache."
  (let ((project-root (or project-root (doom-project-root))))
    (or (and (not refresh-p) (gethash project-root +php-composer-conf))
        (let ((package-file (expand-file-name "composer.json" project-root)))
          (when-let (data (and (file-exists-p package-file)
                               (require 'json)
                               (json-read-file package-file)))
            (puthash project-root data +php-composer-conf))))))

;;;###autoload
(defun +phpunit-toggle-stop-on-error ()
  "Toggle phpunit stop on error"
  (setq phpunit-stop-on-error (not phpunit-stop-on-error)))

;;;###autoload
(defun +phpunit-toggle-stop-on-failure ()
  "Toggle phpunit stop on failure"
  (setq phpunit-stop-on-failure (not phpunit-stop-on-failure)))

;;;###autoload
(defun +phpunit-toggle-stop-on-skipped ()
  "Toggle phpunit stop on skipped"
  (setq phpunit-stop-on-skipped (not phpunit-stop-on-skipped)))

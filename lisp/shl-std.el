;;; shl-std.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Package Functionality
(defun shl::package::install (&rest pkgs)
  "Install pkgs, if not previously installed"
  (dolist (pkg pkgs)
    (unless (package-installed-p pkg)
      package-install pkg)))

(defun shl::package::require (&rest pkgs)
  "Install and require pkgs, if not previously installed"
  (dolist (pkg pkgs)
    (unless (package-installed-p pkg)
      package-install pkg)
    (require pkg)))

;;; TODO: Refactor to add to above functions
(defun shl::package::vc-install (package package-url)
  (unless (package-installed-p package)
    (package-vc-install package-url)))

(defun shl::package::vc-require (package package-url)
  (shl::package::vc-install package package-url)
  (require package))

(provide 'shl-std)
;;; shl-std.el ends here

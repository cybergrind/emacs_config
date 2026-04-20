(setenv "LSP_USE_PLISTS" "true")
(setq package-enable-at-startup nil)

;; Skip the tramp-gvfs backend. When any code (find-file, helm-files, magit...)
;; triggers `(require 'tramp)', tramp auto-probes dbus and loads tramp-gvfs,
;; which adds ~500ms and pulls in dbus/xml/cus-edit. We don't use gvfs-based
;; tramp methods, so short-circuit it: flag off + provide-stub so `require'
;; is a no-op.
(setq tramp-gvfs-enabled nil)
(provide 'tramp-gvfs)

;; Lift GC ceiling during startup; restore sensible values after init. Avoids
;; dozens of GC pauses while faces/themes/custom.el allocate. Measured 2s+ of
;; GC during `emacs -nw' init before this.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 32 1024 1024)
                  gc-cons-percentage 0.1)))

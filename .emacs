;; the t parameter apends to the hook, instead of prepending
;; this means it'd be run after other hooks that might fiddle
;; with the frame size
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; A better way may be is to use prog-mode as a whole instead of doing for all programming language.
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; In order to have line numbers in all buffers and have them persistently
(global-display-line-numbers-mode)

;; (load-theme 'solarized-dark t)
;; (load-theme 'solarized-dark t)
;; (load-theme 'dracula-theme t)

(add-hook 'after-init-hook (lambda () (load-theme 'dracula)))
;; (add-hook 'after-init-hook (lambda () (load-theme 'solarized-light)))
;; (add-hook 'after-init-hook (lambda () (load-theme 'solarized-dark)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("603a831e0f2e466480cdc633ba37a0b1ae3c3e9a4e90183833bc4def3421a961" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" default))
 '(package-selected-packages '(solarized-theme dracula-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; eshell-config.el
;; ############################## Настраиваем eshell ##############################
;; Теперь eshell + eat можно запускать так:
;; M-x eat-eshell

;; Создаем hotkey для eat-eshell:
(global-set-key (kbd "C-c e") #'eat-eshell)

;; Чтобы автоматически использовать Eat в eshell:
(add-hook 'eshell-mode-hook #'eat-eshell-mode)

;; Использование eshell с подсветкой командного вывода
(use-package eshell-syntax-highlighting
  :config
  (add-hook 'eshell-mode-hook 'eshell-syntax-highlighting-mode))

;; Включить поддержку ANSI-цветов в eshell
;; (setq eshell-handle-ansi-color t)
(add-hook 'eshell-mode-hook 'eshell-syntax-highlighting-mode)


(defun my-eshell-clear ()
  "Clear the eshell buffer."
  (interactive)
  (eshell/clear-scrollback)
  (eshell-send-input))

(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map (kbd "C-c M-o") 'my-eshell-clear)))
;; ################################################################################

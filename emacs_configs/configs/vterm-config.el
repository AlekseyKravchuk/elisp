;;; vterm-config.el --- Custom keybindings for vterm -*- lexical-binding: t; -*-

(defun my/vterm-enter-copy-mode ()
  "Automatically enter vterm-copy-mode when pressing C-p or C-n."
  (interactive)
  (unless vterm-copy-mode
    (vterm-copy-mode 1))
  (if (eq last-command 'previous-line)
      (call-interactively 'previous-line)
    (call-interactively 'next-line)))

(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "C-p") 'my/vterm-enter-copy-mode)
  (define-key vterm-mode-map (kbd "C-n") 'my/vterm-enter-copy-mode))

(provide 'vterm-config) ;; Это нужно, чтобы можно было загружать этот файл через require

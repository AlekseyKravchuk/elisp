;; ########################################################################################################
;; Функция require загружает и инициализирует указанный пакет (в данном случае package):
;;     - Проверяет, загружен ли модуль package.
;;     - Если модуль не загружен, загружает его из стандартных библиотек Emacs.
;;     - Если модуль отсутствует, выбрасывает ошибку.
(require 'package)
(setq package-enable-at-startup nil)

;; setq (от 'set quoted') устанавливает значение переменной package-archives
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
	     '("melpa-stable" . "https://stable.melpa.org/packages/")
             )

;; Инициализируем пакетную систему
(package-initialize)

;; Обновляем список доступных пакетов, если он пуст
;; Once you have added your preferred archive, you need to update the local package list using:
;; M-x package-refresh-contents RET
;; and
;; M-x package-list-packages RET
(unless package-archive-contents
  (package-refresh-contents))

;; Устанавливаем use-package, если он ещё не установлен (https://cpp-lang.net/pl/tools/standalone/editors/setup-emacs/#setting-up-use-package)
;; use-package объединяет установку, настройку и загрузку пакетов в одном месте.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Включаем use-package - теперь этот менеджер будет использоваться для загрузки и установки установленных пакетов в emacs.
(require 'use-package)

;; Автоматически устанавливать пакеты, если их нет:
;; use-package по умолчанию не устанавливает пакеты, а только подключает их.
;; Чтобы use-package устанавливал пакет, нужно указывать :ensure t в каждом вызове:
;; (use-package vterm
;;   :ensure t)
;; Однако, если включить use-package-always-ensure, то :ensure t можно не указывать — установка произойдёт автоматически:
(setq use-package-always-ensure t)

(use-package eat)
(when (require 'eat nil t)
  (message "Eat loaded successfully"))

;; the t parameter apends to the hook, instead of prepending
;; this means it'd be run after other hooks that might fiddle with the frame size
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; Display line numbers in every buffer
(global-display-line-numbers-mode)

;; Don't show the splash screen (splash screen - экран-заставка)
(setq inhibit-startup-message t)

;; add columns numbering
(setq column-number-mode t)

;; Change highlight color
(set-face-attribute 'region nil :background "#666")

;; prevent emacs to insert tabs, use spaces instead
(setq-default indent-tabs-mode nil)

;; ############################## использование keybindings в русской раскладке ##############################
(use-package reverse-im
  :custom
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode 1))
;; ############################################################################################################

;; save the backup files in some other directory, where they won't bother you unless you go looking for them
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

;; For now, the only way I found to fix it is to disable signature checking and verify all packages by hand.
;; disable because of elpa bug in emacs 27.1
;; (setq package-check-signature nil)

;; Setting default formatters with use-package
;; https://github.com/lassik/emacs-format-all-the-code?tab=readme-ov-file
(use-package format-all
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters
                '(("C"     (astyle "--mode=c"))
                  ("Shell" (shfmt "-i" "4" "-ci")))))

;; ################################ Установка грамматик для пакета tree-sitter ###############################
;; 1) Настраиваем источники грамматик (список), который указывает Emacs, откуда брать исходники грамматик
;; treesit-language-source-alist - это встроенная переменная Emacs, начиная с версии 29.
;; Она используется функцией treesit-install-language-grammar, чтобы:
;;      - знать, откуда скачать грамматику (ссылка на репозиторий),
;;      - какую ветку и подкаталог использовать для сборки.
(setq treesit-language-source-alist
      '((c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (bash "https://github.com/tree-sitter/tree-sitter-bash")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        ;; (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "main" ".")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (make "https://github.com/alemuller/tree-sitter-make")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown" "master" "markdown")
        (xml "https://github.com/ObserverOfTime/tree-sitter-xml" "master" "xml/src")
       ))
;; Просто задать значение списка "treesit-language-source-alist" недостаточно - это просто список источников.
;; Чтобы использовать грамматики:
;;     - Надо скачать и собрать их (через treesit-install-language-grammar).
;;     - Нужно переключить режимы редактирования на *-ts-mode, чтобы использовать Tree-sitter.

;; 2) Переопределяем стандартные режимы на Tree-sitter версии
(setq major-mode-remap-alist
      '((c-mode          . c-ts-mode)
        (c++-mode        . c++-ts-mode)
        (python-mode     . python-ts-mode)
        (go-mode         . go-ts-mode)
        (sh-mode         . bash-ts-mode)
        (json-mode       . json-ts-mode)
        (yaml-mode       . yaml-ts-mode)
        (toml-mode       . toml-ts-mode)
        (dockerfile-mode . dockerfile-ts-mode)
        (cmake-mode      . cmake-ts-mode)
        (markdown-mode   . markdown-ts-mode)
        ;; xml-mode пока нет - можно добавить отдельно
        ))

;; TODO: почему-то при открытии Docker-файлов, не происходит автоматического переключения на режим "dockerfile-ts-mode"
;; Приходится переключать режим вручную: "M-x dockerfile-ts-mode"
;; 

;; 3) Устанавливаем все грамматики автоматически при запуске Emacs
(dolist (lang '(bash c cpp python go json yaml toml dockerfile make cmake markdown xml))
  (unless (treesit-language-available-p lang)
    (ignore-errors
      (treesit-install-language-grammar lang))))
;; ###########################################################################################################

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(reverse-im format-all eat use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

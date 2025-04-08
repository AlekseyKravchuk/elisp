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

;; Включаем use-package
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

;; ############################## Настройка vterm ##############################
;; Открыть vterm можно с помощью команды: M-x vterm
;; Устанавливаем vterm — мощный терминал для Emacs, основанный на библиотеке libvterm, который поддерживает полнофункциональный терминал с высокой производительностью.
;; Перед установкой vterm необходимо установить библиотеку libvterm. В Ubuntu 24.04 можно установить её так:
;; sudo apt install libvterm-dev cmake
;; Теперь вместо длинного
;; (use-package vterm
;;   :ensure t)
;; Можно использовать более короткую запись, которая установит пакет vterm:
(use-package vterm)
(global-set-key (kbd "C-c t") 'vterm)  ;; Привязываем keybindings для запуска vterm

;; Использование vterm
;;     C-c C-t — переключение между vterm и обычным Emacs-буфером.
;;     C-c C-k — включение режима vterm-copy-mode, позволяющего перемещаться по буферу (C-c C-k снова выключает его).
;;     C-c C-y — вставка содержимого буфера Emacs в vterm

;; Чтобы  интегрировать vterm с zsh внутри Emacs, убедитесь, что ваш vterm-shell установлен правильно:
;; Если Zsh уже настроен в Emacs, vterm автоматически будет использовать файл .zshrc.
;; (setq vterm-shell "/bin/zsh")
(setq vterm-shell "/bin/bash")
;; #############################################################################

;; ############################## Создаем отдельные файлы для хранения настроек пакетов ##############################
;; mkdir -p ~/.emacs.d/configs               ;; Создаем отдельный файл для хранения настроек vterm; флаг "-p" позволяет создать промежуточные каталоги, если они не существуют.
;; touch ~/.emacs.d/configs/vterm-config.el  ;; Создаем файл vterm-config.el

(add-to-list 'load-path "~/.emacs.d/configs/")  ;; Подключаем каталог для хранения конфигураций - добавляем этот каталог в путь загрузки
(load "~/.emacs.d/configs/vterm-config.el")     ;; load загружает и выполняет файл vterm-config.el
(load "~/.emacs.d/configs/eshell-config.el")    ;; load загружает и выполняет файл eshell-config.el
(load "~/.emacs.d/configs/shell-config.el")     ;; load загружает и выполняет файл shell-config.el
;; (require 'vterm-config) ;; Загружаем наш файл vterm-config.el
;; ################################################################################################################

;; ############################## использование keybindings в русской раскладке ##############################
(use-package reverse-im
  :custom
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode 1))
;; ############################################################################################################

;; ################################################## Markdown ################################################
;; Предварительно  нужно установаить пакет pandoc:
;; sudo apt-get install pandoc
;; Пакет Pandoc — это универсальный инструмент для конвертации файлов между различными форматами разметки, такими как Markdown, HTML, LaTeX, PDF и многие другие.
;; Он используется для преобразования текстов и документов в разные форматы, что делает его удобным инструментом в различных рабочих процессах,
;; включая написание технической документации, ведение блогов или создание научных публикаций.
(use-package markdown-mode
  ;; :mode указывает, при каких расширениях файлов активировать этот режим
  :mode ("\\.md\\'" . markdown-mode)
  ;; :init выполняется ДО загрузки markdown-mode
  ;; Здесь мы указываем, что командой для преобразования Markdown в HTML будет 'pandoc' — мощный универсальный конвертер.
  :init
  (setq markdown-command "pandoc"))

;;  Просмотр Markdown в браузере - устанавливаем markdown-preview-mode
(use-package markdown-preview-mode)
;; (use-package markdown-preview-mode
;;   :commands markdown-preview
;;   :init
;;   (setq markdown-preview-stylesheets '("~/.emacs.d/configs/gpt-style.css")))
;; Вызываем M-x markdown-preview-mode => в результате откроется браузер с живым предпросмотром Markdown-документа.

(defun my-markdown-preview ()
  "Предпросмотр Markdown в браузере с кастомным стилем."
  (interactive)
  (let ((output (concat (file-name-sans-extension buffer-file-name) ".html"))
        (css-file "~/.emacs.d/configs/gpt-style.css"))  ;; путь к стилю
    (shell-command (concat "pandoc " buffer-file-name " -o " output " --css=" css-file))
    (browse-url output)))  ;; Открытие HTML в браузере


;; Добавляем ассоциацию, которая говорит Emacs, что все файлы с расширением .md, расположенные в папке it_knowledge (и её подкаталогах), должны открываться в режиме markdown-mode.
;; (add-to-list 'auto-mode-alist '("~/it_knowledge/.*\\.md\\'" . markdown-mode))

;; ;; Включение предпросмотра Markdown через Pandoc
;; (defun my-markdown-preview ()
;;   "Предпросмотр Markdown в браузере."
;;   (interactive)
;;   (let ((output (concat (file-name-sans-extension buffer-file-name) ".html")))
;;     (shell-command (concat "pandoc " buffer-file-name " -o " output))
;;     (browse-url output)))

;; ;; привязываем предпросмотр Markdown через Pandoc к ключу "C-c p"
;; (global-set-key (kbd "C-c p") 'my-markdown-preview)
;; ############################################################################################################

;; the t parameter apends to the hook, instead of prepending
;; this means it'd be run after other hooks that might fiddle with the frame size
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; Display line numbers in every buffer
(global-display-line-numbers-mode)

;; Don't show the splash screen (splash screen - экран-заставка)
(setq inhibit-startup-message t)

;; add columns numbering
(setq column-number-mode t)

;; ############################## theme ##############################
(load-theme 'dracula t)  ;; manual theme installation

;; (load-theme 'dracula)

;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; (load-theme 'jetbrains-darcula t)

;; (add-hook 'after-init-hook (lambda () (load-theme 'dracula)))

;; (load-theme 'darcula t)
;; (add-hook 'after-init-hook (lambda () (load-theme 'darcula)))
;; #######################################################################

;; prevent emacs to insert tabs, use spaces instead
(setq-default indent-tabs-mode nil)

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
(setq package-check-signature nil)

;; Setting default formatters with use-package
;; https://github.com/lassik/emacs-format-all-the-code?tab=readme-ov-file
(use-package format-all
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters
                '(("C"     (astyle "--mode=c"))
                  ("Shell" (shfmt "-i" "4" "-ci")))))

;; Emacs Integration with ClangFormat (located at /usr/bin/clang-format)
(load "/home/kav/.emacs.d/elpa/clang-format-20240115.1750/clang-format.el")
(global-set-key [C-M-tab] 'clang-format-region)

;;################### C++ settings ##################
(use-package lsp-mode
  :ensure t
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :config (setq lsp-completion-enable-additional-text-edit nil))
(use-package lsp-ui
  :commands lsp-ui-mode
  :ensure t)
(use-package ccls
  :ensure t
  :config
  (setq ccls-executable "ccls")
  (setq lsp-prefer-flymake nil)
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))
(use-package flycheck
  :ensure t)
(use-package yasnippet
  :ensure t
  :config (yas-global-mode))
(use-package which-key
  :ensure t
  :config (which-key-mode))
(use-package helm-lsp
  :ensure t)
(use-package helm
  :ensure t
  :config (helm-mode))
(use-package lsp-treemacs
  :ensure t)

;;; This will enable emacs to compile a simple cpp single file without any makefile by just pressing [f9] key
(defun code-compile()
  (interactive)
  (unless (file-exists-p "Makefile")
    (set (make-local-variable 'compile-command)
	 (let ((file (file-name-nondirectory buffer-file-name)))
	   (format "%s -o %s %s"
		   (if (equal (file-name-extension file) "cpp") "g++" "gcc")
		   (file-name-sans-extension file)
		   file)))
    (compile compile-command)))
(global-set-key [f9] 'code-compile)
;;################ END of C++ settings ##############

;;################# GoLang settings #################
;; lsp-mode is an Emacs client for an LSP server
;; lsp-mode installation: M-x package-install RET lsp-mode RET
(require 'lsp-mode)
(require 'go-mode)

(add-hook 'go-mode-hook 'lsp-deferred)
(add-hook 'go-mode-hook 'subword-mode)
(add-hook 'before-save-hook 'gofmt-before-save)

(add-hook 'go-mode-hook (lambda ()
                          (setq tab-width 4)
                          (flycheck-add-next-checker 'lsp 'go-vet)
                          (flycheck-add-next-checker 'lsp 'go-staticcheck)))

;; The path to lsp-mode needs to be added to load-path as well as the
;; path to the `clients' subdirectory.
(add-to-list 'load-path (expand-file-name "lib/lsp-mode" "/home/kav/.emacs.d"))
(add-to-list 'load-path (expand-file-name "lib/lsp-mode/clients" "/home/kav/.emacs.d"))

;; By default, some gopls analyzers are enabled and others are disabled. To override, use:
(setq lsp-go-analyses '((shadow . t)
                        (simplifycompositelit . :json-false)))

;;############### Dockerfile settings ###############
(add-to-list 'load-path "/your/path/to/dockerfile-mode/")
(require 'dockerfile-mode)
;; ##################################################

;; ##################################################
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "4a4bf2300b5170ac1fdc4ecc02e7a6e31e4e2a7f4a6ac3c06f1d1fada8f9a56e" "3f1dcd824a683e0ab194b3a1daac18a923eed4dba5269eecb050c718ab4d5a26" "603a831e0f2e466480cdc633ba37a0b1ae3c3e9a4e90183833bc4def3421a961" default))
 '(format-all-default-formatters
   '(("Assembly" asmfmt)
     ("ATS" atsfmt)
     ("Bazel" buildifier)
     ("BibTeX" emacs-bibtex)
     ("" nil)
     ("" nil)
     ("" nil)
     ("C" clang-format)
     ("C#" csharpier)
     ("C++" clang-format)
     ("Cabal Config" cabal-fmt)
     ("Clojure" zprint)
     ("CMake" cmake-format)
     ("Crystal" crystal)
     ("CSS" prettier)
     ("Cuda" clang-format)
     ("D" dfmt)
     ("Dart" dart-format)
     ("Dhall" dhall)
     ("Dockerfile" dockfmt)
     ("Elixir" mix-format)
     ("Elm" elm-format)
     ("Emacs Lisp" emacs-lisp)
     ("Erlang" efmt)
     ("F#" fantomas)
     ("Fish" fish-indent)
     ("Fortran Free Form" fprettify)
     ("GLSL" clang-format)
     ("Go" gofmt)
     ("GraphQL" prettier)
     ("Haskell" brittany)
     ("HCL" hclfmt)
     ("HLSL" clang-format)
     ("HTML" html-tidy)
     ("HTML+EEX" mix-format)
     ("HTML+ERB" erb-format)
     ("Hy" emacs-hy)
     ("Java" clang-format)
     ("JavaScript" prettier)
     ("JSON" prettier)
     ("JSON5" prettier)
     ("Jsonnet" jsonnetfmt)
     ("JSX" prettier)
     ("Kotlin" ktlint)
     ("LaTeX" latexindent)
     ("Less" prettier)
     ("Literate Haskell" brittany)
     ("Lua" lua-fmt)
     ("Markdown" prettier)
     ("Meson" muon-fmt)
     ("Nix" nixpkgs-fmt)
     ("Objective-C" clang-format)
     ("OCaml" ocp-indent)
     ("Perl" perltidy)
     ("PHP" prettier)
     ("Protocol Buffer" clang-format)
     ("PureScript" purty)
     ("Python" black)
     ("R" styler)
     ("Reason" bsrefmt)
     ("ReScript" rescript)
     ("Ruby" rufo)
     ("Rust" rustfmt)
     ("Scala" scalafmt)
     ("SCSS" prettier)
     ("Shell" shfmt)
     ("Solidity" prettier)
     ("SQL" sqlformat)
     ("Svelte" prettier)
     ("Swift" swiftformat)
     ("Terraform" terraform-fmt)
     ("TOML" prettier)
     ("TSX" prettier)
     ("TypeScript" prettier)
     ("V" v-fmt)
     ("Verilog" istyle-verilog)
     ("Vue" prettier)
     ("XML" html-tidy)
     ("YAML" prettier)
     ("Zig" zig)
     ("_Angular" prettier)
     ("_AZSL" clang-format)
     ("_Beancount" bean-format)
     ("_Caddyfile" caddy-fmt)
     ("_Flow" prettier)
     ("_Gleam" gleam)
     ("_Ledger" ledger-mode)
     ("_Nginx" nginxfmt)
     ("_Snakemake" snakefmt)))
 '(package-selected-packages
   '(markdown-preview-mode reverse-im iso-transl eshell-syntax-highlighting eat vterm jetbrains-darcula-theme darcula-theme helm-lsp dockerfile-mode go-mode lsp-mode use-package clang-format format-all solarized-theme modus-themes dracula-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

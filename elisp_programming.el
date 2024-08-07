;; https://alexott.net/ru/emacs/elisp-intro/elisp-intro-ru.html - Программирование на Emacs Lisp
;; https://alexott.net/ru/cpp/BoostAsioNotes.html               - Что такое Boost.Asio и зачем он нужен
;; https://alexott.net/ru/cpp/BoostAsioProxy.html               - Как написать простой HTTP прокси используя Boost.Asio
;; https://alexott.net/ru/cpp/                                  - Языки C, C++ и связанные с ним вещи
;; https://alexott.net/ru/writings/emacs-vcs/EmacsGit.html      - Работа с Git
;; https://alexott.net/ru/writings/emacs-devenv/EmacsCedet.html - Использование Emacs вместе с CEDET
;; https://alexott.net/ru/writings/prog-checking/               - Поиск утечек памяти, и прочих ошибок в программах
;; https://developers.redhat.com/blog/2021/05/05/memory-error-checking-in-c-and-c-comparing-sanitizers-and-valgrind - Memory error checking in C and C++: Comparing Sanitizers and Valgrind



;; Самый простой список в elisp:
;; Для того, чтобы вычислить выражение (в данном случае - список), нужно расположить курсор сразу за выражением и запустить команду
;; "eval-last-sexp" (C-x C-e)
'(cat dog fish)
(cat dog fish)  ;; Debugger entered--Lisp error: (void-function cat)

;; Количество пробелов в списке не имеет значения, поэтому список ниже равен списку, приведенному выше

(setq lst1 '(one two three))
lst1

(setq lst2 '(cat
             dog
             fish)
      )

;; append function returns the new value, it does not modify its arguments. So we need to use setq:
(setq lst3 (append lst2 '(chicken turtle)))
lst3

;; push instead modifies its arguments AND adds to the beginning of the list, not the end!
(push 'strange_animal lst3)
lst3

(add-to-list)

;; Список в Лиспе (любой список) - это программа, готовая к запуску.
;; Если вы запустите ее (evaluate it), то компьютер сделает следующее:
;; 1) ничего не сделает, а только вернет вам сам список;
;; 2) отобразит сообщение об ошибке;
;; 3) обработает первый символ в списке как команду сделать что-либо. (Обычно, именно последнее - это то, что вы хотите на самом деле!)

;; Одиночный апостроф ' называют цитатой (quote) и, когда перед списком стоит апостроф, это сигнал для Лиспа ничего не делать со списком, а взять его как есть.
'(again simple elisp list see apostrophe before it)

;; Но если перед списком нет апостофа, то первый символ в списке имеет особое значение - ТЕПЕРЬ это команда, которую компьютер должен выполнить.
;; В Лиспе эти команды называют функциями.
;; Пробуем вычислить список: располагаем курсор сразу после правой скобки списка  и нажимаем сочетание клавиш C-x C-e:
;; В эхо-области появится число 100.
(+ 10 20 30 40) ;; Перед этим списком нет апострофа => первый элемент списка, '+', elisp будет рассматривать как инструкцию сделать что-то с остальными элементами списка

;; Пример пустого списка:
() ;; Если вычислить(evaluate) пустой список, то в эхо-области будет отображено "nil"

;; Пытаюсь создать список со строками
'("word1" "string, containing multiple words" "dog" "cat")
("word1" "string, containing multiple words" "dog" "cat")

(concat "string" "word2" "dog" "cat")
(string-join '("string" "word2" "dog" "cat") "-")

'(1 2 3 4 5)
(1 2 3 4 5)  ;; invalid function 1

(string-join '("word1" "word2" "dog" "cat") "-")

(concat "aba" "caba")

(string-join '("aba" "caba") "-")

(string-join '("aba"
	       "caba"
	       "daba") ", ")

;;Функция cons создает, или конструирует списки;
(cons 'word '(home car bag))
(cons "word" '("home" "car" "bag"))

'word


(concat "aba"
	"caba")

(substring "abcde" 0 2)

(substring "0123456789" 4 2)

;; ============== 1.5 Интерпретатор Лиспа ==============
;; Что делает интерпретатор Лиспа, когда мы даем ему команду вычислить выражение?
;; Первым делом интерпретатор смотрит, есть ли апостроф перед списком; если да, то он только возвращает выражение как есть.
;; Если же апострофа нет, то интерпретатор анализирует первый элемент списка и пытается определить по этому символу определение функции.
;; Если функция существует, то интерпретатор выполняет соответствующие ей инструкции. В противном случае он выдает сообщение об ошибке.


;; https://alexott.net/ru/emacs/elisp-intro/elisp-intro-ru_3.html#SEC20
;; ============== 1.7 Переменные ==============
;; Кроме списков, интерпретатор Лиспа может вычислить символ, перед которым нет апострофа, и который не заключен в круглые скобки.
;; В этом случае интерпретатор Лиспа попытается определить значение символа, считая его переменной.
(setq s "0123456789")

s  ;; вычисляем символ, считая его переменной, т.к. перед символом нет апострофа
's ;; вычисляем символ как обычный символ, т.к. перед ним стоит апостроф


;; Если вы попытаетесь вычислить символ, с которым не связано никакое значение, то вы получите сообщение об ошибке.
non_defined_var

(set 'defined_var 255)
defined_var

(setq defined_var 255) ;; то же самое, что и функция set, но не нужно ставить апостроф перед defined_var
defined_var

(setq lst '("abc" "efg" "hij"))
lst

(substring s 4 7)  ;; 456

;; Переменная fill-column --- пример символа, которому соответствует некоторое значение
;; (в каждом буфере GNU Emacs этому символу присвоено какое-нибудь значение, обычно 70 или 72.) 
fill-column

;; При использовании set, вам нужно ставить апостроф перед обеими аргументами set, если вы не хотите, чтобы интерпретатор вычислял их
(set 'list_of_names '("Alex" "Nikita" "Olga" "Mike"))
list_of_names

(+ 2 4 10 22)

;; Если вы попытаетесь вычислить символ, с которым не связано никакое значение, то вы получите сообщение об ошибке.
(set 'list_of_names '("Alex" "Nikita" "Olga" "Mike"))

;; Эта особая форма действует точно также, как и set, но перед первым аргументом НЕ надо ставить апостроф, так как он не вычисляется. 
(setq list_of_names '("Alex" "Nikita" "Olga" "Mike"))

(setq var 'word)

;; 04.01.2024
;; 2. Практика вычислений : https://alexott.net/ru/emacs/elisp-intro/elisp-intro-ru_4.html#SEC34
buffer-name ;; Debugger entered--Lisp error: (void-variable buffer-name)

(buffer-name)
(buffer-file-name)


;; C-x C-e runs the command eval-last-sexp
;; eval-last-sexp is an interactive compiled Lisp function.
;; It is bound to <menu-bar> <emacs-lisp> <Evaluate Last S-expression>, C-x C-e.
;; Функции, которые вы вычисляете нажатием клавиш, называются `интерактивными' функциями, или командами.

;; 1.8.5 Функция message
(message "Это сообщение появится в эхо-области!")

;; Однако если в строке найдется символ `%s', то функция message не будет печатать его, а вместо этого просмотрит аргументы, которые следуют за строкой.
(message "Имя этого буфера: %s." (buffer-name))

buffer-name   ;; Debugger entered--Lisp error: (void-variable buffer-name)
(buffer-name) ;; buffer-name is a built-in function in ‘C source code’.

;; 1.9 Присваиваем переменной значение
;; Есть несколько способов, для того чтобы переменной можно было присвоить какое-нибудь значение.
;; Например, с помощью функций set и setq. Или с помощью функции let (see section 3.6 let). (На Лисп жаргоне это называют связать переменную со значением.)

;; TODO:
;; 1.9.1 Используя set
;; Чтобы связать с символом цветы список '(роза фиалка маргаритка лютик), вычислите следующее выражение, расположив курсор после него и нажав C-x C-e.
 	
(set 'цветы  '(роза фиалка маргаритка лютик))
(set цветы  '(роза фиалка маргаритка лютик))

;; Useful link on learning elisp and emacs
;; ~/cpp_code/multithreading/01_02_thread_management_using_RAII/


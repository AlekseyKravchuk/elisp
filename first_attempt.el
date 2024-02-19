;; Список в Лиспе (любой список) --- это программа, готовая к запуску.
;; Если вы запустите ее (на жаргоне Лиспа вычислите), то компьютер сделает следующее:
;; 1) ничего не сделает, а только вернет вам сам список;
;; 2) отобразит сообщение об ошибке;
;; 3) обработает первый символ в списке как команду сделать что-либо. (Обычно, именно последнее --- это то, что вы хотите на самом деле!) 

;; Одиночный апостроф ' называют цитатой (quote);
;; когда перед списком стоит апостроф, это сигнал для Лиспа ничего не делать со списком, а взять его как есть.
'(1 2 3 4 5 6 7 (10 20 30)) ;; we can evaluate this expression => в эхо-области появится список (1 2 3 4 5 6 7 (10 20 30)) (но уже без апострофа)

;;если перед списком нет апостофа, то первый символ в списке имеет особое значение: --- это команда, которую компьютер должен выполнить. (В Лиспе эти команды называют функциями).
(+ 1 2 3 4 5 6 7 8 9
   10 11 12 13)  ;; при этом первый элемент списка-команда будет применена ко все остальным элементам

(concat "one" "two")
(message (concat "one" "two"))

(message "Имя этого буфера: %s" (buffer-name))

(message "Значение переменной fill-column: %d" fill-column)

;; Присваиваем переменной значение
(set 'list-name '(1 2 3 4 5 ))
list-name
'list-name

;;
(setq lst '(10 20 30))
lst
'lst


(setq var1 '(11 12 13 14)
      var2 15
      var3 "word")
var1
var2
var3


(setq counter 0)
(setq counter (+ counter 1))
counter
(setq counter (+ counter 1))
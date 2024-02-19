(defun hello_world()
  ("Hello, world"))

(defun do-some-math (x y)
    (* (+ x 20)
       (- y 10)))

(defun print_name(name)
  (message "Your name is %s. Congratulations, %s!" name))

(do-some-math 100 50)

(hello-world)

(print_name(Aleksey)

'(+ 1 2 3 4 5 6)
 
(+ 2 fill-column)

(setq counter 10)
counter
(setq counter (+ counter 15))

(+ 2 counter)

counter
;; index_from = 6; desired_len_of_substr = 5 / index_from = 6; index_to_exclusive = 11
(setq str "panamabanana")

(length str)
(substring string_to_test 6 (- (length str) 6))
(length str)
(- (length str) 6)
(substring str 6 11)

(+ 3 4 5)
(+ 2 'hello)

'(это список перед которым стоит апостроф)

(это список перед которым стоит апостроф)

(+ 2 100)

;; (setq words '(это список перед которым стоит апостроф))
(setq name "Aleksey")

(message "Hello, %s. That is funny that you are interested in %s" name "Emacs Lisp")

(message "Имя текущего буфера: %s." (buffer-name))

;; Когда вы выполняете какую-нибудь команду редактирования, такую как перемещение курсора или прокрутка экрана, вы вычисляете выражение,
;; первым элементом которого является функция. Именно так и работает Emacs.
(buffer-name)
(buffer-file-name)
default-directory  ;;

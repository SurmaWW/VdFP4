<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right">Студент: Сурмачевський Владислав Володимирович<p>
<p align="right">Рік: 2024<p>
  
# Завдання складається з двох частин:
#### 1. Переписати функціональну реалізацію алгоритму сортування з лабораторної роботи 3 з такими змінами: використати функції вищого порядку для роботи з послідовностями (де це доречно); додати до інтерфейсу функції (та використання в реалізації) два ключових параметра: key та test , що працюють аналогічно до того, як працюють параметри з такими назвами в функціях, що працюють з послідовностями. При цьому key має виконатись мінімальну кількість разів.
#### 2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за варіантом (див. п 4.1.2). Використання псевдо-функцій не забороняється, але, за можливості, має бути мінімізоване.

## Завдання 1
### Варіант 2: Алгоритм сортування обміном No1 (без оптимізацій) за незменшенням.
  
#### Лістинг програми

    (defun bubble-sort (sequence &key (key #'identity) (test #'<=))
      (let* ((len (length sequence))
             (arr (map 'vector #'identity sequence))
             (key-cache (make-hash-table :test 'eql)))
        
        (labels ((get-key (item)
                   (or (gethash item key-cache)
                       (setf (gethash item key-cache)
                             (funcall key item))))
                 
                 (should-swap (a b)
                   (not (funcall test (get-key a) (get-key b)))))
          
          (dotimes (i (1- len))
            (dotimes (j (- len i 1))
              (when (should-swap (aref arr j) (aref arr (1+ j)))
                (rotatef (aref arr j) (aref arr (1+ j))))))
          
          (coerce arr (type-of sequence)))))

#### Тестування функції

    (defun check-bubble-sort (name input expected &key (key #'identity) (test #'<=))
      (format t "~:[FAILED~;passed~]... ~a~%"
              (equal (bubble-sort input :key key :test test) expected)
              name))
    
    (defun test-bubble-sort ()
      (check-bubble-sort "Test 1" '(3 1 2) '(1 2 3))
      (check-bubble-sort "Test 2" '(1 2 3) '(1 2 3))
      (check-bubble-sort "Test 3" '(5 4 3 2 1) '(1 2 3 4 5))
      (check-bubble-sort "Test 4" '(4 1 3 2 4 1) '(1 1 2 3 4 4))
      (check-bubble-sort "Test 5" '(-3 -1 2 5 -4) '(-1 2 -3 -4 5) :key #'abs)
      (check-bubble-sort "Test 6" '(1 2 3 4 5) '(5 4 3 2 1) :test #'>)
      (check-bubble-sort "Test 7" '() '())
      (check-bubble-sort "Test 8" '(42) '(42)))

    (test-bubble-sort)


#### Результат

    (test-bubble-sort)
    
    passed... Test 1
    passed... Test 2
    passed... Test 3
    passed... Test 4
    passed... Test 5
    passed... Test 6
    passed... Test 7
    passed... Test 8

## Завдання 2
### Варіант 6
Написати функцію rpropagation-reducer , яка має один ключовий параметр — функцію
comparator . rpropagation-reducer має повернути функцію, яка при застосуванні в
якості першого аргумента reduce робить наступне: при обході списку з кінця, якщо
елемент списку-аргумента reduce не "кращий" за попередній (той, що "справа") згідно з
comparator , тоді він заміняється на значення попереднього, тобто "кращого", елемента.
Якщо ж він "кращий" за попередній елемент згідно comparator , тоді заміна не
відбувається. Функція comparator за замовчуванням має значення #'< . Обмеження,
які накладаються на використання функції-результату rpropagation-reducer при
передачі у reduce визначаються розробником (тобто, наприклад, необхідно чітко
визначити, якими мають бути значення ключових параметрів функції reduce from-end
та initial-value ).

    CL-USER> (reduce (rpropagation-reducer)
                    '(3 2 1 2 3)
                    :from-end ...
                    :initial-value ...)
    (1 1 1 2 3)
    CL-USER> (reduce (rpropagation-reducer)
                    '(3 1 4 2)
                    :from-end ...
                    :initial-value ...)
    (1 1 2 2)
    CL-USER> (reduce (rpropagation-reducer :comparator #'>)
                    '(1 2 3)
                    :from-end ...
                    :initial-value ...)
    (3 3 3)
#### Лістинг програми

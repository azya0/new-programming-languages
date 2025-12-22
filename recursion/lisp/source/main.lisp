(defpackage :partitions
  (:use :common-lisp)
  (:export :generate-partitions))

(in-package :partitions)

(defun generate-partitions (n)
  (labels ((recursive-partitions (remaining max-partition current result)
             (cond
               ((zerop remaining)
                (cons (reverse current) result))
               ((< remaining 0)
                result)
               (t
                (loop for i from (min max-partition remaining) downto 1
                      do (setq result (recursive-partitions (- remaining i)
                                                           i
                                                           (cons i current)
                                                           result)))
                result))))
    (recursive-partitions n n nil nil)))

(defun format-partition (partition)
  (format nil "(~{~d~^ ~})" partition))

(defun test-partitions ()
  (format t "Разбиения для N=4:~%")
  (let ((partitions4 (generate-partitions 4)))
    (dolist (p partitions4)
      (format t "  ~a~%" (format-partition p)))
    (format t "Всего: ~d разбиений~%~%" (length partitions4)))
  
  (format t "Разбиения для N=5:~%")
  (let ((partitions5 (generate-partitions 5)))
    (dolist (p partitions5)
      (format t "  ~a~%" (format-partition p)))
    (format t "Всего: ~d разбиений~%~%" (length partitions5)))
  
  (format t "Тест для различных N:~%")
  (dotimes (i 7)
    (let* ((n (+ i 1))
           (partitions (generate-partitions n)))
      (format t "N=~d: ~d разбиений~%" n (length partitions))))
  
  (let ((test-n 6))
    (format t "~%Пример для N=~d (первые 10 разбиений):~%" test-n)
    (let ((partitions (generate-partitions test-n)))
      (dotimes (i (min 10 (length partitions)))
        (format t "  ~a~%" (format-partition (nth i partitions)))))))

(defun main ()
  (test-partitions))

(main)
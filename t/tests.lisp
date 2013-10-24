(defpackage :conspath.test
  (:use #:cl #:conspath #:checkl))
(in-package :conspath.test)

(check (:name :star)
  (match '(:a * :b)
    '((:a
       (:b x (:b n))
       (:c (:d (:x q f (:b y z))))))))

(check (:name :car)
  (match '(:a (car :b))
    '((:a (:b b) (:b x) (:c c)))))

(check (:name :or)
  (match '(:a (or (:b) (:c)))
    '((:a
       (:b x (:b n))
       (:c (:b y z))))))

(check (:name :plus)
  (match '(:a (+ :b))
    '((:a
       (:b x (:b n))
       (:c (:b y z))))))

(check (:name :and :output-p t)
  (match '(:a (and (:b) (* :c)))
    '((:a
       (:b x (:b n))
       (:b (:d (:c y z)))))))

(check (:name :index :output-p t)
  (match '(:a 2)
    '((:a
       (0 1 2)
       (1 2 3)
       (2 2 2)))))

(check (:name :atom)
  (match '(:a (atom :b))
    '((:a :b :c :b))))

(defpackage :conspath
  (:use #:cl #:alexandria)
  (:export

   #:match

   #:*current-conspath*
   #:match-complex #:match-next #:sub-match #:sub-match-list #:found))

(defpackage :conspath.matchers
  (:use #:cl))

(in-package :conspath)

(defvar *current-conspath* nil)
(defvar *matches* nil)

(defgeneric match-simple (match item)
  (:documentation "Match a simple, non-cons MATCH, and check it against ITEM.
Return a match, or NIL for no match."))

(defgeneric match-complex (match args item)
  (:documentation "Match a complex match, MATCH, with args ARGS, against
ITEM."))

(defun match-next (list)
  (%match (cdr *current-conspath*) list))

(defun sub-match-list (conspath list &optional (collect-p t))
  (if collect-p
      (%match conspath list)
      (let (*matches*)
        (%match conspath list)
        *matches*)))

(defun sub-match (match item &optional (collect-p t))
  (car (sub-match-list match (list item) collect-p)))

(defun found (item)
  (push item *matches*))

(defun %match (conspath list)
  (let ((*current-conspath* conspath))
    (if (null (car conspath))
        (match-simple nil list)
        (mapcar (lambda (x) (match-simple (car conspath) x)) list))))

(defun match (conspath list)
  (let (*matches*)
    (%match conspath list)
    (nreverse *matches*)))

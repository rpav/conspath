(in-package :conspath)

(defmethod match-simple (match item) nil)
(defmethod match-simple ((match null) item)
  (found item))

(defmethod match-simple ((match function) item)
  (funcall match item))

(defmethod match-simple ((match symbol) (item list))
  (when (eq match (car item))
    (match-next (cdr item))))

(defmethod match-simple ((match integer) item)
  (match-next (elt item match)))

(defmethod match-simple ((match list) item)
  (match-complex (car match) (cdr match) item))

(defmethod match-simple ((match (eql 'cl:*)) item)
  (match-next (list item))
  (when (and (listp item) (cdr item))
    (sub-match-list *current-conspath* (cdr item))))

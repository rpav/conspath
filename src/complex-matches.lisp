(in-package :conspath)

(defmethod match-complex ((match (eql 'function)) args item)
  (funcall (car args) item))

(defmethod match-complex ((match (eql 'cl:quote)) args item)
  (if (equal (car args) item)
      (found args)
      (when (listp item)
        (match-next item))))

(defmethod match-complex ((match (eql 'cl:car)) args item)
  (when (sub-match args item nil)
    (found (cadr item))))

(defmethod match-complex ((match (eql 'cl:or)) args item)
  (some #'identity
        (mapcar (lambda (x) (sub-match x item)) args)))

(defmethod match-complex ((match (eql 'cl:and)) args item)
  (when (every #'identity
               (mapcar (lambda (x)
                         (:say x)
                         (sub-match x item nil)) args))
    (found item)))

(defmethod match-complex ((match (eql 'cl:*)) args item)
  (if (sub-match args item nil)
      (sub-match (list* 'cl:* args) (cdr item))
      (match-next item)))

(defmethod match-complex ((match (eql 'cl:+)) args item)
  (when (sub-match args item nil)
    (sub-match (list* 'cl:* args) (cdr item))))

(defmethod match-complex ((match (eql 'cl:atom)) args item)
  (when (equalp (car args) item) (found item)))

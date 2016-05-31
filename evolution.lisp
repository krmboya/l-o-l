(defparameter *width* 100)
(defparameter *height* 30)
(defparameter *jungle* '(45 10 10 10))  ; x, y, width, height  - top left corner
(defparameter *plant-energy* 80)

;; hash table that stores plant location by coordinate
;; use function equal for matching keys
(defparameter *plants* (make-hash-table :test #'equal))

;; Creates plants randomly in specified region of map
(defun random-plant (left top width height)
   (let ((pos (cons (+ left (random width)) (+ top (random height)))))
        (setf (gethash pos *plants*) t)))

;; Adds a plant inside jungle and outside of it each time it runs
(defun add-plants ()
   (apply #'random-plant *jungle*)
   (random-plant 0 0 *width* *height*))

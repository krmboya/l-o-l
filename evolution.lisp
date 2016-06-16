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

;; Structure describing animal
(defstruct animal x y energy dir genes)

;; initialize variable animals with a list containing one animal
(defparameter *animals* 
    (list (make-animal :x      (ash *width*  -1)
                       :y      (ash *height* -1)
                       :energy 1000
                       :dir    0
                       :genes  (loop repeat 8
				  collecting (1+ (random 10))))))


(defun move (animal)
  (let ((dir (animal-dir animal))
        (x (animal-x animal))
        (y (animal-y animal)))
    (setf (animal-x animal) (mod (+ x
                                    (cond ((and (>= dir 2) (< dir 5)) 1)
                                          ((or (= dir 1) (= dir 5)) 0)
                                          (t -1))
                                    *width*)
                                 *width*))
    (setf (animal-y animal) (mod (+ y
                                    (cond ((and (>= dir 0) (< dir 3)) -1)
                                          ((and (>= dir 4) (< dir 7)) 1)
                                          (t 0))
                                    *height*)
                                 *height*))
    (decf (animal-energy animal))))

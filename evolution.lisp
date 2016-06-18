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

;; initialize animals with a list containing one animal
(defparameter *animals* 
    (list (make-animal :x      (ash *width*  -1)
                       :y      (ash *height* -1)
                       :energy 1000
                       :dir    0
                       :genes  (loop repeat 8
				  collecting (1+ (random 10))))))

;; change an animals x, y coordinate based on current direction
(defun move (animal)
  (let ((dir (animal-dir animal))
        (x (animal-x animal))
        (y (animal-y animal)))
    ;; direction indicators (animal at the center)
    ;;  0 1 2
    ;;  7 a 3
    ;;  6 5 4
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


(defun turn (animal)
  (let ((x (random (apply #'+ (animal-genes animal)))))
    (labels ((angle (genes x)
               (let ((xnu (- x (car genes))))
                 (if (< xnu 0)
                     0
                     (1+ (angle (cdr genes) xnu))))))
        (setf (animal-dir animal)
              (mod (+ (animal-dir animal) (angle (animal-genes animal) x)) 8)))))

(defparameter *width* 100)
(defparameter *height* 30)
(defparameter *jungle* '(45 10 10 10))  ; x, y, width, height  - top left corner
(defparameter *plant-energy* 80)

;; hash table that stores plant location by coordinate
;; use function equal for matching keys
(defparameter *plants* (make-hash-table :test #'equal))

;; Creates plant randomly in specified region of map
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


;; Changes an animal's current direction based on genes
(defun turn (animal)
  ;; x - random value btn 0 and sum of all genes
  (let ((x (random (apply #'+ (animal-genes animal)))))
    ;; Returns a value btn 0 and 7 to indicate turning direction
    (labels ((angle (genes x)
               (let ((xnu (- x (car genes))))
                 (if (< xnu 0)
                     0
                     (1+ (angle (cdr genes) xnu))))))
        (setf (animal-dir animal)
              (mod (+ (animal-dir animal) (angle (animal-genes animal) x)) 8)))))


;; Increment animals energy if plant exists in position
(defun eat (animal)
  (let ((pos (cons (animal-x animal) (animal-y animal))))
    (when (gethash pos *plants*)
      (incf (animal-energy animal) *plant-energy*)
      (remhash pos *plants*))))


(defparameter *reproduction-energy* 200)

;; Creates a copy of animal (with mutated genes)
(defun reproduce (animal)
  (let ((e (animal-energy animal)))
    (when (>= e *reproduction-energy*)  ;; only reproduce if above threshold
      (setf (animal-energy animal) (ash e -1))  ;; halve the animal energy
      (let ((animal-nu (copy-structure animal))  ;; shallow copy of animal
            (genes     (copy-list (animal-genes animal)))
            (mutation  (random 8)))  ;; pick random gene to mutate
	;; mutate selected gene to +/- 1 original value
        (setf (nth mutation genes) (max 1 (+ (nth mutation genes) (random 3) -1)))
        (setf (animal-genes animal-nu) genes)
        (push animal-nu *animals*)))))


;; Updates the world
(defun update-world ()
  ;; remove dead animals
  (setf *animals* (remove-if (lambda (animal)
                                 (<= (animal-energy animal) 0))
                             *animals*))
  ;; perform daily animal activities
  (mapc (lambda (animal)
          (turn animal)
          (move animal)
          (eat animal)
          (reproduce animal))
        *animals*)
  (add-plants))

;; Draws a  snapshot of the world (plant, animal location)
(defun draw-world ()
  (loop for y
     below *height*
     ;; begin row
     do (progn (fresh-line)
	       (princ "|")
	       (loop for x
		  below *width*
		  ;; M if at least 1 animal exists
		  ;; * if a plant exists
		  ;; space otherwise
		  do (princ (cond ((some (lambda (animal)
					   (and (= (animal-x animal) x)
						(= (animal-y animal) y)))
					 *animals*)
				   #\M)
				  ((gethash (cons x y) *plants*) #\*)
				  (t #\space))))
	       (princ "|"))))

;; Evolve world according to user input
(defun evolution ()
  (draw-world)
  (fresh-line)
  (let ((str (read-line)))
    (cond ((equal str "quit") ())  ;; quit command
          (t (let ((x (parse-integer str :junk-allowed t)))
               (if x
		   ;; x is an integer, evolve the world x times
                   (loop for i
                      below x
                      do (update-world)
                      if (zerop (mod i 1000))
                      do (princ #\.)) ;; print . after every thousandth
		   ;; update only once
                   (update-world))
               (evolution))))))

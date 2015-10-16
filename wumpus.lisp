(load "graph-util")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

;; Returns a random node (range 1 to *node-num*)
(defun random-node ()
  (1+ (random *node-num*)))

;; Returns 2 directed edges btn two nodes
(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

;; generate random *edge-num* pairs of edges
(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
		     collect (edge-pair (random-node) (random-node)))))

;; Finds all direct edges to `node`
(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
		   (eql (car x) node))
		 edge-list))

;; Finds all nodes connected to `node`
;; DFS
(defun get-connected (node edge-list)
  (let ((visited nil))
    (labels ((traverse (node)
	       (unless (member node visited)
		 (push node visited)
		 (mapc (lambda (edge)
			 (traverse (cdr edge)))
		       (direct-edges node edge-list)))))
      (traverse node))
    visited))

;; Return a list of islands amongst `nodes`
(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
	       (let* ((connected (get-connected (car nodes) edge-list))
		      (unconnected (set-difference nodes connected)))
		 (push connected islands)
		 (when unconnected
		   (find-island unconnected)))))
      (find-island nodes))
    islands))

;; Connects `islands` with bridges
(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
	    (connect-with-bridges (cdr islands)))))

;; Connects `islands` in `nodes` with bridges
(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

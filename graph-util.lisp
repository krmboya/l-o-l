;; substitutes non-alphanumeric characters in node names to give valid DOT names
(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))
  ;; prin1, output suitable for input to read, no newlines

(defparameter *max-label-length* 30)

;; generate a label of *max-label-length* that should appear on a graph node
(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
	(if (> (length s) *max-label-length*)
	    (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
	    s))
      ""))

;; generate dot info from nodes
(defun nodes->dot (nodes)
  (mapc (lambda (node)  ;; mapc, results of applying the function are not accumulated
	  (fresh-line)
	  (princ (dot-name (car node)))  ;; princ, output suitable for people (no escape chars)
	  (princ "[label=\"")
	  (princ (dot-label node))
	  (princ "\"]"))
	nodes))

;; generate dot info from edges (directed)
(defun edges->dot (edges)
  (mapc (lambda (node)
	  (mapc (lambda (edge)
		  (fresh-line)
		  (princ (dot-name (car node)))
		  (princ "->")
		  (princ (dot-name (car edge)))
		  (princ "[label=\"")
		  (princ (dot-label (cdr edge)))
		  (princ "\"]"))
		(cdr node)))
	edges))

;;(maplist (lambda (foo) (print foo)) '(1 2 3))
;;
;;(1 2 3) 
;;(2 3) 
;;(3) 
;;((1 2 3) (2 3) (3))

;; generate dot info from edges (undirected)
(defun uedges->dot (edges)
  (maplist (lambda (lst)
	     (mapc (lambda (edge)
		     (unless (assoc (car edge) (cdr lst))
		       (fresh-line)
		       (princ (dot-name (caar lst)))
		       (princ "--")
		       (princ (dot-name (car edge)))
		       (princ "[label=\"")
		       (princ (dot-label (cdr edge)))
		       (princ "\"];")))
		   (cdar lst)))
	   edges))

;; generate all dot data
(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))


;; turn the dot file into a picture
;; requires the graphviz package with the executable `/usr/bin/dot`
(defun dot->png (fname thunk)
  (with-open-file (*standard-output* ;; temporarily bind to different stream
		   fname
		   :direction :output
		   :if-exists :supersede)
    (funcall thunk))
  (sb-ext:run-program "/usr/bin/dot"  `("-Tpng" "-O"  ,fname) :output *standard-output*))

;; turn whole graph into a picture
;; fname is a full path to the output file
(defun graph->png (fname nodes edges)
  (dot->png fname
	    (lambda ()
	      (graph->dot nodes edges))))


;; generate dot info of an undirected graph
(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

;; generate an undirected graph
(defun ugraph->png (fname nodes edges)
  (dot->png fname
	    (lambda ()
	      (ugraph->dot nodes edges))))

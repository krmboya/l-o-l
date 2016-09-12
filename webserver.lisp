; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; version 2 of the License.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; Partial Author: Conrad Barski, M.D.
; Parts Adapted with permission from http.lisp by Ron Garret

(defun decode-param (s)
  "Recursively decodes a URL encoded string"
  (labels ((f (lst)
	     (when lst
	       ;; check the first character
	       (case (car lst)

		 ;; if % convert following hex number to a character i.e.
		 ;; next two characters
		 ;; if invalid, returns nil (junk-allowed)
		 (#\% (cons (code-char (parse-integer (coerce (list 
							       (cadr lst) 
							       (caddr lst)) 
							      'string) 
						      :radix 16 
						      :junk-allowed t))
			    (f (cdddr lst))))
		 ;; if + replace with space
		 (#\+ (cons #\space (f (cdr lst))))
		 (otherwise (cons (car lst) (f (cdr lst))))))))
    ;; convert string to list, process it, then back to string
    (coerce (f (coerce s 'list)) 'string)))

(defun parse-params (s)
  "Returns list of conses each representing a key value pair from query params"
   (let* ((i1 (position #\= s)) ;; pos of =
          (i2 (position #\& s))) ;; pos of &
      (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
                            (decode-param (subseq s (1+ i1) i2)))
		      ;; recursively parse rest of string generating conses
		      ;; or evaluate to nil
                      (and i2 (parse-params (subseq s (1+ i2))))))
            ((equal s "") nil) ;; if string is blank return nil
            (t s))))  ;; otherwise return the string itself

(defun parse-url (s)
  "Returns the path and query parameters from the Request header"
  (let* ((url (subseq s
		      (+ 2 (position #\space s))
		      (position #\space s :from-end t))) ;;the path component
	 (x (position #\? url)))  ;;location of ? signaling query parameters
    ;; Return list containing path component, and optionally the
    ;; parsed query parameters
    (if x
	(cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
	(cons url '()))))

(defun get-header (stream)
  "Returns a list of header/value pairs in stream"
  (let* ((s (read-line stream)) ;; read in line from stream
         (h (let ((i (position #\: s))) ;; pos of delimiter :
	      (when i 
		(cons (intern (string-upcase (subseq s 0 i))) ;; header symbol
		      (subseq s (+ i 2)))))))  ;; value of header
    (when h
      ;; prepend header/value to output of rest of stream
      (cons h (get-header stream)))))

(defun get-content-params (stream header)
  "Returns the parsed request body if exists"
  (let ((length (cdr (assoc 'content-length header))))
    (when length  ;; content-length header specified
      ;; make a string equal in length to the content-length header
      (let ((content (make-string (parse-integer length))))
	(read-sequence content stream)  ;; fill up the string from stream
	(parse-params content)))))  ;; parse the string


(defun serve (request-handler)
  "Serves a http request"
  (let ((socket (socket-server 8080)))  ;; bind to port 8080
    (unwind-protect  ;; use unwind-protect to ensure socket is always closed
	 ;; webserver loop
	 ;; bind stream to incoming socket data
	 (loop (with-open-stream (stream (socket-accept socket))
		 ;; parse components of request
                 (let* ((url    (parse-url (read-line stream)))
                        (path   (car url))
                        (header (get-header stream))
                        (params (append (cdr url) 
                                        (get-content-params stream header)))
			;; redefine stdout to point to stream
                        (*standard-output* stream))
		   ;; invoke provided request handler with parsed request data
                   (funcall request-handler path header params))))
      (socket-server-close socket))))  ;; finally, close socket


(defun hello-request-handler (path header params)
  "Handler for path greeting"
  (if (equal path "greeting")
      (let ((name (assoc 'name params))) ;; find param name in request params
        (if (not name)
            (princ "<form>What is your name?<input name='name' /></form>")
            (format t "Nice to meet you, ~a!" (cdr name))))
      (princ "Sorry... I don't know that page.")))

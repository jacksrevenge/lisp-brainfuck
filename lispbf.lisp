;;;; Brainfuck compiler written in lisp
;;;; + - increment/decrement byte
;;;; < > move pointer left/right
;;;; . output byte
;;;; , input byte
;;;; [ if byte is 0, jump to ]
;;;; ] if byte not 0, jump back to [

(defparameter *byte-array* (make-array '(10) :adjustable t :initial-element 0))

(defparameter *pointer* 0)

(defparameter *instructions* (string "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."))

(setq matching-brackets (make-hash-table))
(setq stack (list))
(dotimes (i (length *instructions*))
  (if (char= #\[ (aref *instructions* i))
      (push i stack))
  (if (char= #\] (aref *instructions* i))
      (progn
	(setq opening-index (pop stack))
	(setf (gethash opening-index matching-brackets) i)
	(setf (gethash i matching-brackets) opening-index))))

(dotimes (step-pointer (length *instructions*))
  (setq step (aref *instructions* step-pointer))
  (case step
    (#\+ (progn
	   (setf (aref *byte-array* *pointer*) (+ (aref *byte-array* *pointer*) 1))
	   (if (> (aref *byte-array* *pointer*) 255)
	       (setf (aref *byte-array* *pointer*) 0))))
    (#\- (progn
	   (setf (aref *byte-array* *pointer*) (- (aref *byte-array* *pointer*) 1))
	   (if (< (aref *byte-array* *pointer*) 0)
	       (setf (aref *byte-array* *pointer*) 255))))
    (#\> (progn
	   (setf *pointer* (+ *pointer* 1))
	   (if (= step-pointer 9)
	       (adjust-array *byte-array* (+ (array-total-size *byte-array*) 10) :initial-element 0))))
    (#\< (setf *pointer* (- *pointer* 1)))
    (#\. (format T "~a" (code-char (aref *byte-array* *pointer*))))
    (#\, (setf (aref *byte-array* *pointer*) (char-code (read-char))))
    (#\[ (if (= (aref *byte-array* *pointer*) 0)
	     (setf step-pointer (gethash step-pointer matching-brackets))))
    (#\] (if (/= (aref *byte-array* *pointer*) 0)
	     (setf step-pointer (gethash step-pointer matching-brackets))))
    (otherwise nil)))





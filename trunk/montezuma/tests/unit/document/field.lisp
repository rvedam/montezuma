(in-package #:montezuma)

;; Some simple unit test utilities

(defparameter *passed-tests* '())
(defparameter *failed-tests* '())

(defmacro test (name expr expected-value &optional (comparator '(function equal))
		failure-code)
  `(unless (test-aux ',name ',expr ,expr ,expected-value ,comparator)
    ,failure-code))

(defmacro condition-test (name expr expected-condition &optional (comparator '(function typep))
			  failure-code)
  (let ((completed-var (gensym "COMPLETED"))
	(condition-var (gensym "CONDITION"))
	(value-var (gensym "VALUE")))
    `(let ((,completed-var NIL))
       (multiple-value-bind (,value-var ,condition-var)
	   (ignore-errors
	     ,expr
	     (setf ,completed-var T))
	 (unless (condition-test-aux ',name ',expr ,value-var (not ,completed-var)
				     ,condition-var ,expected-condition ,comparator)
	   ,failure-code)))))

(defun condition-test-aux (name expr value error-p error expected-error comparator)
  (if error-p
      (let ((got-expected-p (funcall comparator error expected-error)))
	(if got-expected-p
	    (test-success name expr error expected-error)
	    (test-failure name expr error expected-error))
	got-expected-p)
      (test-failure name expr value expected-error)))

(defun test-aux (name expr value expected-value comparator)
  (let ((got-expected-p (funcall comparator value expected-value)))
    (if got-expected-p
	(test-success name expr value expected-value)
	(test-failure name expr value expected-value))
    got-expected-p))

(defun test-failure (name expr value expected-value)
  (assert (not (assoc name *failed-tests*)))
  (assert (not (assoc name *passed-tests*)))
  (push (cons name (list expr value expected-value)) *failed-tests*)
  (warn "FAILURE: Test ~S: ~S evaluated to ~S instead of ~S."
	name expr value expected-value)
  nil)

(defun test-success (name expr value expected-value)
  (assert (not (assoc name *failed-tests*)))
  (assert (not (assoc name *passed-tests*)))
  (push (cons name (list expr value expected-value)) *passed-tests*)
  (format T "~&Test ~S passed.~%" name))

(defun begin-tests ()
  (setf *passed-tests* '())
  (setf *failed-tests* '()))

(defun end-tests ()
  (let ((num-failed (length *failed-tests*))
	(num-passed (length *passed-tests*)))
    (format T "~&-----~&Testing complete, ~S of ~S tests failed (~,2F)"
	    num-failed
	    (+ num-failed num-passed)
	    (/ num-failed (+ num-failed num-passed)))))



(defun run-tests ()
  (test-standard-field)
  (test-set-store)
  (test-set-index)
  (test-set-term-vector)
  (test-new-binary-field)
  T)


(defun test-standard-field ()
  (let ((f (make-field "name" "value" :stored :compress :index :tokenized)))
    (test standard-field-1 (field-name f) "name")
    (test standard-field-2 (field-data f) "value")
    (test standard-field-3 (field-stored-p f)     T)
    (test standard-field-4 (field-compressed-p f) T)
    (test standard-field-5 (field-indexed-p f)    T)
    (test standard-field-6 (field-tokenized-p f)  T)
    (test standard-field-7 (field-store-term-vector-p f)  NIL)
    (test standard-field-8 (field-store-offsets-p f)  NIL)
    (test standard-field-9 (field-store-positions-p f)  NIL)
    (test standard-field-10 (field-omit-norms-p f)  NIL)
    (test standard-field-11 (field-binary-p f)  NIL)))

(defun test-set-store ()
  (let ((f (make-field "name" nil :stored :compress :index :tokenized)))
    (setf (field-stored f) NIL)
    (test set-store-1 (field-stored-p f) NIL)
    (test set-store-2 (field-compressed-p f) NIL)))


(defun test-set-index ()
  (let ((f (make-field "name" "value" :stored :compress :index :tokenized)))
    (setf (field-index f) NIL)
    (test set-index-1 (field-indexed-p f) NIL)
    (test set-index-2 (field-tokenized-p f) NIL)
    (test set-index-3 (field-omit-norms-p f) NIL)
    (setf (field-index f) :no-norms)
    (test set-index-4 (field-indexed-p f) T)
    (test set-index-5 (field-tokenized-p f) NIL)
    (test set-index-6 (field-omit-norms-p f) T)))

(defun test-set-term-vector ()
  (let ((f (make-field "name" "value" :stored :compress :index :tokenized)))
    (setf (field-store-term-vector f) :with-positions-offsets)
    (test set-term-vector-1 (field-store-term-vector-p f) T)
    (test set-term-vector-2 (field-store-offsets-p f) T)
    (test set-term-vector-3 (field-store-positions-p f) T)))

(defun test-new-binary-field ()
  (let ((bin (make-array (list 256) :element-type '(unsigned-byte 8))))
    (dotimes (i 256)
      (setf (aref bin i) i))
    (let ((f (make-binary-field "name" bin T)))
      (test new-binary-field-1 (field-name f) "name" #'string=)
      (test new-binary-field-2 (field-data f) bin)
      (test new-binary-field-3 (field-stored-p f) T)
      (test new-binary-field-4 (field-compressed-p f) NIL)
      (test new-binary-field-5 (field-indexed-p f) NIL)
      (test new-binary-field-6 (field-store-term-vector-p f) NIL)
      (test new-binary-field-7 (field-store-offsets-p f) NIL)
      (test new-binary-field-8 (field-store-positions-p f) NIL)
      (test new-binary-field-9 (field-omit-norms-p f) NIL)
      (test new-binary-field-10 (field-binary-p f) T))))



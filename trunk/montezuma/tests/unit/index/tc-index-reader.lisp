(in-package #:montezuma)

(defun do-test-term-doc-enum (ir)
  (atest term-doc-enum *index-test-helper-ir-test-doc-count* (num-docs ir))
  (atest term-doc-enum *index-test-helper-ir-test-doc-count* (max-doc ir))
  (let ((term (make-term "body" "Wally")))
    (atest term-doc-enum (doc-freq ir term) 4)
    (let ((tde (term-docs-for ir term)))
      (atest term-doc-enum (and (next tde) T) T)
      (atest term-doc-enum (doc tde) 0)
      (atest term-doc-enum (freq tde) 1)
      (atest term-doc-enum (and (next tde) T) T)
      (atest term-doc-enum (doc tde) 5)
      (atest term-doc-enum (freq tde) 1)
      (atest term-doc-enum (and (next tde) T) T)
      (atest term-doc-enum (doc tde) 18)
      (atest term-doc-enum (freq tde) 3)
      (atest term-doc-enum (and (next tde) T) T)
      (atest term-doc-enum (doc tde) 20)
      (atest term-doc-enum (freq tde) 6)
      (atest term-doc-enum (next tde) NIL)
      
      ;; Test fast read.  Use a small array to exercise repeat read.
      (let ((docs (make-array 3))
	    (freqs (make-array 3))
	    (term (make-term "body" "read")))
	(seek tde term)
	(atest term-doc-enum (read tde docs freqs) 3)
	(atest term-doc-enum docs #(1 2 6) #'equal)
	(atest term-doc-enum freqs #(1 2 4) #'equal)
	(atest term-doc-enum (read tde docs freqs) 3)
	(atest term-doc-enum docs #(9 10 15) #'equal)
	(atest term-doc-enum freqs #(3 1 1) #'equal)
	(atest term-doc-enum (read tde docs freqs) 1)
	(atest term-doc-enum (subseq docs 0 1) #(21) #'equal)
	(atest term-doc-enum (subseq freqs 0 1) #(6) #'equal)
	(atest term-doc-enum (read tde docs freqs) 0)
	(do-test-term-docpos-enum-skip-to ir tde)
	(close tde))

      ;; Test term positions
      (let* ((term (make-term "body" "read"))
	     (tde (term-positions-for ir term)))
	(atest term-doc-enum (and (next tde) T) T)
	(atest term-doc-enum (doc tde) 1)
	(atest term-doc-enum (freq tde) 1)
	(atest term-doc-enum (next-position tde) 3)
	(atest term-doc-enum (and (next tde) T) T)
	(atest term-doc-enum (doc tde) 2)
	(atest term-doc-enum (freq tde) 2)
	(atest term-doc-enum (next-position tde) 1)
	(atest term-doc-enum (next-position tde) 4)
	(atest term-doc-enum (and (next tde) T) T)
	(atest term-doc-enum (doc tde) 6)
	(atest term-doc-enum (freq tde) 4)
	(atest term-doc-enum (next-position tde) 3)
	(atest term-doc-enum (next-position tde) 4)
	(atest term-doc-enum (and (next tde) T) T)
	(atest term-doc-enum (doc tde) 9)
	(atest term-doc-enum (freq tde) 3)
	(atest term-doc-enum (next-position tde) 0)
	(atest term-doc-enum (next-position tde) 4)
	(atest term-doc-enum (and (skip-to tde 16) T) T)
	(atest term-doc-enum (doc tde) 16)
	(atest term-doc-enum (freq tde) 2)
	(atest term-doc-enum (next-position tde) 2)
	(atest term-doc-enum (and (skip-to tde 21) T) T)
	(atest term-doc-enum (doc tde) 21)
	(atest term-doc-enum (freq tde) 6)
	(atest term-doc-enum (next-position tde) 3)
	(atest term-doc-enum (next-position tde) 4)
	(atest term-doc-enum (next-position tde) 5)
	(atest term-doc-enum (next-position tde) 8)
	(atest term-doc-enum (next-position tde) 9)
	(atest term-doc-enum (next-position tde) 10)
	(atest term-doc-enum (next tde) NIL)
	
	(test-term-docpos-enum-skip-to ir tde)
	(close tde)))))


	
(deftestfixture segment-reader-test
  (:vars dir ir)
  (:setup
   (setf (fixture-var 'dir) (make-instance 'ram-directory))
   (let ((iw (make-instance 'index-writer
			    :directory (fixture-var 'dir)
			    :analyzer (make-instance 'whitespace-analyzer)
			    :create-p T))
	 (docs (index-test-helper-prepare-ir-test-docs)))
     (dotimes (i *index-test-helper-ir-test-doc-count*)
       (add-document-to-index-writer iw (aref docs i)))))
  (:teardown
   (close (fixture-var 'ir))
   (close (fixture-var 'dir))))

     
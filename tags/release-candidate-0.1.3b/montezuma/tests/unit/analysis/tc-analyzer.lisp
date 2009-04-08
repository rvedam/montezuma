(in-package #:montezuma)

(deftestfun test-analyzer
  (with-input-from-string (input "JJWiseman@yahoo.com is My E-Mail 523@#$ ADDRESS. 23#@$")
    (let* ((analyzer (make-instance 'analyzer))
	   (token-stream (token-stream analyzer "fieldname" input)))
      (test analyzer-1 (next-token token-stream) (make-token "jjwiseman" 0 9) #'token=)
      (test analyzer-2 (next-token token-stream) (make-token "yahoo" 10 15) #'token=)
      (test analyzer-3 (next-token token-stream) (make-token "com" 16 19) #'token=)
      (test analyzer-4 (next-token token-stream) (make-token "is" 20 22) #'token=)
      (test analyzer-5 (next-token token-stream) (make-token "my" 23 25) #'token=)
      (test analyzer-6 (next-token token-stream) (make-token "e" 26 27) #'token=)
      (test analyzer-7 (next-token token-stream) (make-token "mail" 28 32) #'token=)
      (test analyzer-8 (next-token token-stream) (make-token "address" 40 47) #'token=)
      (test analyzer-9 (next-token token-stream) nil))))

;;; ------------------------------------------------- -*- Mode: LISP -*-

(in-package :asdf)

(defsystem #:montezuma
    :name "Montezuma"
    :author "John Wiseman <jjwiseman@yahoo.com>"
    :maintainer "John Wiseman <jjwiseman@yahoo.com>"
    :version "0.1"
    :licence "MIT"
    :description ""
    :long-description ""

    :components ((:file "package")
		 (:module "document"
			  :components ((:file "field"))
			  :depends-on ("package"))))

(defmethod perform ((o test-op) (c (eql (find-system '#:montezuma))))
  (oos 'load-op '#:montezuma-tests)
  (oos 'test-op '#:montezuma-tests :force t))



(defsystem #:montezuma-tests
  :depends-on (#:montezuma)
  :components ((:module "tests"
			:components ((:module "unit"
					      :components ((:module "document"
								    :components ((:file "field")))))))))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (find-system '#:montezuma-tests))))
  (or (funcall (intern (symbol-name '#:run-tests)
                       (find-package '#:montezuma)))
      (error "test-op failed")))

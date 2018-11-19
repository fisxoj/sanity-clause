(defpackage hax.rove-with-coverage
  (:use #:cl)
  (:export #:run))

(in-package :hax.rove-with-coverage)

(defun run (target &key (style rove:*default-reporter*) (env rove:*default-env*))
    (cl-coveralls:with-coveralls ()
      (rove:run target :style style :env env)))

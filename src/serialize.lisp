(defpackage sanity-clause.serialize
  (:use #:cl
        #:alexandria))

(in-package #:sanity-clause.serialize)

(defgeneric serialize (field serializer value))


(defclass json-serializer ()
  ())

(defmacro defserializer (format ))

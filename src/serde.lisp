(defpackage sanity-clause.serde
  (:use #:cl
        #:alexandria))

(in-package #:sanity-clause.serialize)

(defvar *serializers* nil
  "Plist of known serializers.")

(defgeneric serialize (field serializer value))

(defgeneric deserialize (field serializer value))

(defmacro defserializer (format &key
				  (name-to-lisp #'identity)
				  (lisp-to-name #'identity)))

.. image:: https://travis-ci.org/fisxoj/sanity-clause.svg?branch=master
   :target: https://travis-ci.org/fisxoj/sanity-clause
   :alt: Travis CI status badge
.. image:: https://coveralls.io/repos/github/fisxoj/sanity-clause/badge.svg?branch=master
   :target: https://coveralls.io/github/fisxoj/sanity-clause?branch=master
   :alt: Coveralls status badge


:Source: `https://github.com/fisxoj/sanity-clause <https://github.com/fisxoj/sanity-clause>`_
:Docs:  `https://fisxoj.github.io/sanity-clause/ <https://fisxoj.github.io/sanity-clause/>`_

..

  There's no such thing as Sanity Clause.

  -- Groucho Marx


Sanity clause is a data validation/contract library.  You might use it for configuration data, validating an api response, or documents from a datastore.  In a dynamically typed langauge, it helps you define clearly defined areas of doubt and uncertainty.  We should love our users, but we should never blindly trust their inputs.

To make use of it, you define schemas, which can be property lists with symbols for keys and instances of :class:`sanity-clause.field:field` subclasses that dictate the type of values you expect as well as the shape of the property list to be returned after deserializing and validating data.  For example::

   (list :name (make-field :string) :age (make-field :integer))

You can load these sorts of schemas from a file by writing them as sexps with keywords, like this::

  schema.sexp

  (:key (:string :validator (:not-empty) :default "potato")
   :key2 (:integer :validator ((:int :min 0)) :default 2))

and then loading them using :function:`sanity-clause.loadable-schema:load` to load them.


Finally, you can also define class-based schemas using :class:`sanity-clause:validated-metaclass` like::

   (defclass person ()
        ((favorite-dog :type symbol
                       :field-type :member
                       :members (:wedge :walter)
                       :initarg :favorite-dog
                       :required t)
         (age :type (integer 0)
              :initarg :age
              :required t)
         (potato :type string
                 :initarg :potato
                 :required t))
        (:metaclass sanity-clause:validated-metaclass))

which will validate thier initargs when you instantiate them (**BUT NOT WHEN YOU SET SLOTS**).  Hopefully, that will be added eventually, perhaps as an optional feature.


~~~~~~~
Example
~~~~~~~

``v2-info.json``::

  {
    "title": "Swagger Sample App",
    "description": "This is a sample server Petstore server.",
    "termsOfService": "http://swagger.io/terms/",
    "contact": {
      "name": "API Support",
      "url": "http://www.swagger.io/support",
      "email": "support@swagger.io"
    },
    "license": {
      "name": "Apache 2.0",
      "url": "http://www.apache.org/licenses/LICENSE-2.0.html"
    },
    "version": "1.0.1"
  }


``example.lisp``::

  ;; load required libraries
  (ql:quickload '(jonathan sanity-clause))

  (defclass contact-object ()
    ((name :type string
           :initarg :name
           :documentation "The identifying name of the contact person/organization.")
     (url :type string
          :field-type :uri
          :initarg :url
          :documentation "The URL pointing to the contact information. MUST be in the format of a URL.")
     (email :type string
            :field-type :email
            :initarg :email
            :documentation "The email address of the contact person/organization. MUST be in the format of an email address."))
    (:metaclass sanity-clause:validated-metaclass))


  (defclass license-object ()
    ((name :type string
           :initarg :name
           :documentation "The license name used for the API.")
     (url :type string
          :field-type :uri
          :initarg :url
          :documentation "A URL to the license used for the API. MUST be in the format of a URL."))
    (:metaclass sanity-clause:validated-metaclass))


  (defclass info-object ()
    ((title :type string
            :data-key "title"
            :initarg :title
            :required t
            :documentation "The title of the application.")
     (description :type string
                  :initarg :description
                  :documentation "A short description of the application. GFM syntax can be used for rich text representation.")
     (terms-of-service :type string
                       :data-key "termsOfService"
                       :initarg :terms-of-service
                       :documentation "The Terms of Service for the API.")
     (contact :type contact-object
              :field-type :nested
              :element-type contact-object
              :initarg :contact
              :documentation "The contact information for the exposed API.")
     (license :type license-object
              :field-type :nested
              :element-type license-object
              :initarg :license
              :documentation "The license information for the exposed API.")
     (version :type string
              :initarg :version
              :documentation "Provides the version of the application API (not to be confused with the specification version)."
              :required t))
    (:metaclass sanity-clause:validated-metaclass))

        ;;; Deserialize the json from the file into instances of these classes

  (let ((v2-info (alexandria:read-file-into-string "v2-info.json")))
    (sanity-clause:load (find-class 'info-object) (jojo:parse v2-info :as :alist)))

  ;; => #<INFO-OBJECT {10045F9C93}>

  (slot-value * 'license)

  ;; => #<LICENSE-OBJECT {1006600BE3}>

  (slot-value * 'name)

  ;; => "Apache 2.0"

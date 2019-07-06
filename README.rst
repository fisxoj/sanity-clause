.. image:: https://travis-ci.org/fisxoj/sanity-clause.svg?branch=master
   :target: https://travis-ci.org/fisxoj/sanity-clause
   :alt: Travis CI status badge
.. image:: https://coveralls.io/repos/github/fisxoj/sanity-clause/badge.svg?branch=master
   :target: https://coveralls.io/github/fisxoj/sanity-clause?branch=master
   :alt: Coveralls status badge


:Source: `https://github.com/fisxoj/sanity-clause <https://github.com/fisxoj/sanity-clause>`_
:Docs:  `https://fisxoj.github.io/sanity-clause/ <https://fisxoj.github.io/sanity-clause/>`_

  There's no such thing as Sanity Clause
  -- Groucho Marx


Sanity clause is a data validation/contract library.  You might use it for configuration data, validating an api response, or documents from a datastore.  In a dynamically typed langauge, it helps you define clearly defined areas of doubt and uncertainty.  We should love our users, but we should never blindly trust their inputs.

To make use of it, you define schemas, which can be property lists with symbols for keys and instances of :class:`sanity-clause.field:field` subclasses that dictate the type of values you expect as well as the shape of the property list to be returned after deserializing and validating data.  For example::

   (list :name (make-field :string) :age (make-field :integer))

You can load these sorts of schemas from a file by writing them as sexps with keywords, like this::

  schema.sexp

  (:key (:string :validator (:not-empty) :default "potato")
   :key2 (:integer :validator ((:int :min 0)) :default 2))

and then loading them using :function:`sanity-clause.loadable-schema:load` to load them.


Finally, you can also define class-based schemas using :class:`sanity-clause.metaclass:validated-metaclass` like::

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
        (:metaclass sanity-clause.metaclass:validated-metaclass))

which will validate thier initargs when you instantiate them (**BUT NOT WHEN YOU SET SLOTS**).  Hopefully, that will be added eventually, perhaps as an optional feature.

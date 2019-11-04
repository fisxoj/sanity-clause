;; load required libraries
(ql:quickload '(jonathan sanity-clause))

(defclass contact-object ()
  ((name :type string
         :data-key "name"
         :documentation "The identifying name of the contact person/organization.")
   (url :type string
        :data-key "url"
        :field-type :uri
        :documentation "The URL pointing to the contact information. MUST be in the format of a URL.")
   (email :type string
          :data-key "email"
          :field-type :email
          :documentation "The email address of the contact person/organization. MUST be in the format of an email address."))
  (:metaclass sanity-clause:validated-metaclass))


(defclass license-object ()
  ((name :type string
         :data-key "name"
         :documentation "The license name used for the API.")
   (url :type string
        :data-key "url"
        :field-type :uri
        :documentation "A URL to the license used for the API. MUST be in the format of a URL."))
  (:metaclass sanity-clause:validated-metaclass))


(defclass info-object ()
  ((title :type string
          :data-key "title"
          :required t
          :documentation "The title of the application.")
   (description :type string
                :data-key "description"
                :documentation "A short description of the application. GFM syntax can be used for rich text representation.")
   (terms-of-service :type string
                     :data-key "termsOfService"
                     :documentation "The Terms of Service for the API.")
   (contact :type contact-object
            :field-type :nested
            :data-key "contact"
            :element-type contact-object
            :documentation "The contact information for the exposed API.")
   (license :type license-object
            :field-type :nested
            :element-type license-object
            :data-key "license"
            :documentation "The license information for the exposed API.")
   (version :type string
            :documentation "Provides the version of the application API (not to be confused with the specification version)."
            :data-key "version"
            :required t))
  (:metaclass sanity-clause:validated-metaclass))

;;; Deserialize the json from the file into instances of these classes

(let ((v2-info (alexandria:read-file-into-string (merge-pathnames "v2-info.json" *load-truename*))))
  (sanity-clause:load 'info-object (jojo:parse v2-info :as :alist)))

;; => #<INFO-OBJECT {10045F9C93}>

(slot-value * 'license)

;; => #<LICENSE-OBJECT {1006600BE3}>

(slot-value * 'name)

;; => "Apache 2.0"

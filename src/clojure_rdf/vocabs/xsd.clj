(ns clojure-rdf.vocabs.xsd
  (:refer-clojure :exclude (boolean float double time
                                    long int short byte))
  (:use clojure-rdf.namespace))

(use-prefix "xsd" "http://www.w3.org/2001/XMLSchema#")

(defresources string boolean decimal float double duration dateTime
time date gYearMonth gYear gMonthDay gDay gMonth hexBinary
base64Binary anyURI QName NOTATION normalizedString token language
NMTOKEN NMTOKENS Name NCName ID IDREF IDREFS ENTITY ENTITIES integer
nonPositiveInteger negativeInteger long int short byte
nonNegativeInteger unsignedLong unsignedInt unsignedShort unsignedByte
positiveInteger)

(ns clojure-rdf.vocabs.dcterms
  (:refer-clojure :exclude (type format))
  (:use clojure-rdf.namespace))

(use-prefix "dcterms" "http://purl.org/dc/terms/")

(defresources abstract, accessRights, accrualMethod,
accrualPeriodicity, accrualPolicy, alternative, audience, available,
bibliographicCitation, conformsTo, contributor, coverage, created,
creator, date, dateAccepted, dateCopyrighted, dateSubmitted,
description, educationLevel, extent, format, hasFormat, hasPart,
hasVersion, identifier, instructionalMethod, isFormatOf, isPartOf,
isReferencedBy, isReplacedBy, isRequiredBy, issued, isVersionOf,
language, license, mediator, medium, modified, provenance, publisher,
references, relation, replaces, requires, rights, rightsHolder,
source, spatial, subject, tableOfContents, temporal, title, type,
valid)
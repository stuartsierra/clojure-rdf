(ns clojure-rdf.vocabs.dcterms
  (:refer-clojure :exclude (type format))
  (:use clojure-rdf.namespace))

(use-prefix "dcterms" "http://purl.org/dc/terms/")

;; Properties
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

;; Vocabulary encoding schemes
(defresources DCMIType, DDC, IMT, LCC, LCSH, MESH, NLM, TGN, UDC)

;; Syntax encoding schemes
(defresources Box, ISO3166, ISO639-2, ISO639-3, Period, Point,
RFC1766, RFC3066, RFC4646, URI, W3CDTF)

;; Classes
(defresources Agent, AgentClass, BibliographicResource, FileFormat,
Frequency, Jurisdiction, LicenseDocument, LinguisticSystem, Location,
LocationPeriodOrJurisdiction, MediaType, MediaTypeOrExtent,
MethodOfAccrual, MethodOfInstruction, PeriodOfTime, PhysicalMedium,
PhysicalResource, Policy, ProvenanceStatement, RightsStatement,
SizeOrDuration, Standard)

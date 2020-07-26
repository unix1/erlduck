# Changelog

## 0.3.0

The API has major backward incompatible changes. It has been simplified and JSON decoding removed.

#### Added

* Option to format response in XML
* New functions `erlduck:answer/1-3`

#### Removed

* `erlduck:get_answer/1-2` functions
* `erlduck:search/1-2` functions

#### Changed

* Answers (API responses) are no longer decoded JSON; they are now raw binary.
Consumers are expected to decode the binary responses, whether XML or JSON.
matches
=======

This is a simple library for processing of match events.
Its core resides in `matches.MatchHistory` with the following signature.

* `add(m: MatchEvent): Option[ConsistencyError]`
* `queryLast: Option[MatchEvent]`
* `queryMultiple(n: Int): List[MatchEvent]`
* `queryAll: List[MatchEvent]`

The `matches.MatchEvent` type is a simple case class that can be parsed
from a binary representation using the `matches.MatchHelper.parse` function.

* `parse(input: Long): Either[ValidationError, MatchEvent]`

## Building the project

This is a simple `sbt` project.

To verify its basic correctness it should be sufficient to run `sbt test`.

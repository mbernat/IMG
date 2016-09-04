package matches


abstract class ConsistencyError
case class OldEventError(m: MatchEvent) extends ConsistencyError
case class HistoryError() extends ConsistencyError
case class MissingEventError() extends ConsistencyError
case class DuplicateError() extends ConsistencyError
case class UnknownError() extends ConsistencyError

class MatchHistory() {
  // Querying history only needs to support showing most recent events
  // therefore a list with the most recent entry in the head is a sufficient
  // representation.
  var history: List[MatchEvent] = List()

  // This function detects consistency errors between the new match event
  // and the past history. In partcular, it makes sure that both
  // time and scores are correct.
  //
  // In case there is an error it also tries to determine
  // what exactly went wrong (see @ConsistencyError@ type above).
  def getConsistencyError(m: MatchEvent): Option[ConsistencyError] = {
    if (history.length == 0) return None

    val prevEvent = history(0)

    if (prevEvent == m)
      return Some(DuplicateError())

    if (m.time < prevEvent.time)
      // This lookup is linear in the length of the history,
      // so if we expect many errors of this kind in the input data
      // a history representation with better asymptotics would be in order.
      return history.find(x => MatchHelper.timelessEqual(x, m)) match {
        case Some(oldEvent) => Some(OldEventError(oldEvent))
        case None => Some(UnknownError())
    }

    def newPoints(t: Team) =
      if (m.whoScored == t) m.pointsScored else 0

    val newPointsTotal1 = prevEvent.pointsTotal1 + newPoints(Team1())
    val newPointsTotal2 = prevEvent.pointsTotal2 + newPoints(Team2())

    if (newPointsTotal1 == m.pointsTotal1 &&
        newPointsTotal2 == m.pointsTotal2)
      None
    else if (newPointsTotal1 <= m.pointsTotal1 &&
             newPointsTotal2 <= m.pointsTotal2)
      Some(MissingEventError())
    else if (newPointsTotal1 > m.pointsTotal1 ||
             newPointsTotal2 > m.pointsTotal2)
      Some(HistoryError())
    else
      Some(UnknownError())
  }

  // Add a new event @m@ to match history and return `true`, if possible.
  // If the event is inconsistent with the history we try a number
  // of heuristics to fix the situation. In case it does not help
  // we return `false`.
  def add(m: MatchEvent): Boolean =
    getConsistencyError(m) match {
      // No error, so simply prepend the event to history.
      case None => {
        history = m :: history
        true
      }

      // There was a consistency error but in some cases it might be fixable.
      case Some(r) => r match {
        // If the new event is a duplicate we need no further handling.
        case DuplicateError() => false

        // The new event is already present in the history, possibly
        // with an incorrect timestamp (that we guessed previously
        // in @MissingEventError@ branch below). So we fix the timestamp here.
        case OldEventError(oldMatch) => {
          history = history.map(y =>
            if (MatchHelper.timelessEqual(y, m)) m else y)
          true
        }

        // We are probably missing an event in the history, so try
        // reconstructing it.
        case MissingEventError() =>
          MatchHelper.intermediate(m, history(0)) match {
            // Intermediate event could not be constructed, so report an error.
            case None => false
            // Intermediate event has been constructed, so add both it
            // and the original event to the history.
            case Some(inter) => {
              history = m :: inter :: history
              true
            }
          }

        // History is inconsistent with the new event, so try removing
        // the tip of the history and readding the new event.
        case HistoryError() => {
          history = history.drop(1)
          add(m)
        }

        case UnknownError() => false
      }
    }

  // Retrieve the last match event.
  // Returns @None@ in case the history is empty.
  def queryLast: Option[MatchEvent] = history.headOption

  // Retrieve up to last @n@ match events.
  def queryMultiple(n: Int): List[MatchEvent] = history.take(n)

  // Retrieve complete match history.
  def queryAll: List[MatchEvent] = history
}

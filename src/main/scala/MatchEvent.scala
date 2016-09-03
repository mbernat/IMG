package matches


abstract class Team
case class Team1() extends Team
case class Team2() extends Team

abstract class ValidationError
case class InvalidPointsScored() extends ValidationError

case class MatchEvent(
  time: Short,
  pointsTotal1: Short,
  pointsTotal2: Short,
  whoScored: Team,
  pointsScored: Byte
)

object MatchHelper {
  // Extracts a subsequence of @input@ number in a binary representation.
  // Variable @from@ specifies the position of the highest bit while
  // variable @to@ the position of the lowest bit, inclusive.
  def extract(input: Long, from: Byte, to: Byte): Long =
    (input % (1 << from + 1)) >> to

  // Attempt to parse @input@ number into a valid MatchEvent representation.
  def parse(input: Long): Either[ValidationError, MatchEvent] = {
    val time = extract(input, 30, 19).toShort
    val pointsTotal1 = extract(input, 18, 11).toShort
    val pointsTotal2 = extract(input, 10, 3).toShort
    val whoScored = if (extract(input, 2, 2) == 0) Team1() else Team2()
    val pointsScored = extract(input, 1, 0).toByte

    // The value of @pointsScored@ ranges from 0 to 3 but 0 is not allowed.
    if (pointsScored == 0)
      Left(InvalidPointsScored())
    else
      Right(
        MatchEvent(time, pointsTotal1, pointsTotal2, whoScored, pointsScored))
  }

  // Test two matches for equality while ignoring their timestamps.
  def timelessEqual(a: MatchEvent, b: MatchEvent): Boolean =
    a.pointsTotal1 == b.pointsTotal1 &&
    a.pointsTotal2 == b.pointsTotal2 &&
    a.whoScored == b.whoScored &&
    a.pointsScored == b.pointsScored

  // Try to reconstruct an intermediate event given two other events.
  // Two events contain enough information to infer an intermediate
  // event in case it is missing. The only thing that cannot be
  // be derived is the intermediate timestamp, so we set it arbitrarily
  // to the average of the events' timestamps.
  def intermediate(m: MatchEvent, before: MatchEvent): Option[MatchEvent] = {
    val prevPoints1 = m.whoScored match {
      case Team1() => m.pointsScored
      case Team2() => 0
    }
    val prevPoints2 = m.whoScored match {
      case Team1() => 0
      case Team2() => m.pointsScored
    }

    val prevPointsTotal1 = m.pointsTotal1 - prevPoints1
    val prevPointsTotal2 = m.pointsTotal2 - prevPoints2
    val time = ((m.time + before.time) / 2).toShort

    def mkIntermediate(diff: Int, t: Team): Option[MatchEvent] =
      if (diff > 0 && diff <= 3)
        Some(MatchEvent(time,
          prevPointsTotal1.toShort,
          prevPointsTotal2.toShort,
          Team2(),
          diff.toByte
        ))
      else
        None

    if (prevPointsTotal1 == before.pointsTotal1) {
        val diff = prevPointsTotal2 - before.pointsTotal2
        return mkIntermediate(diff, Team2())
    }

    if (prevPointsTotal2 == before.pointsTotal2) {
        val diff = prevPointsTotal1 - before.pointsTotal1
        return mkIntermediate(diff, Team1())
    }

    None
  }
}

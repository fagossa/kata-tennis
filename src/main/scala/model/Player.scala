package model

case class Player(name: String) {

  var score = 0

  def winBall(): Unit = score += 1

  def description = score match {
    case 0 => "love"
    case 1 => "fifteen"
    case 2 => "thirty"
    case 3 => "forty"
  }

}

case class TennisGame(first: Player, second: Player) {

  def moreThan(value: Int) = first.score + second.score >= value

  def alreadyAWinner: Option[Player] = {
    if (moreThan(6)) {
      if (first.score > 4 && first.score == second.score + 2)
        return Some(first)
      else if (second.score > 4 && second.score == first.score + 2)
          return Some(second)
    }
    None
  }

  def hasAdvantage: Boolean = Math.abs(first.score - second.score) >= 1

  def leader = if (first.score > second.score) first else second

  def score = {
    alreadyAWinner match {
      case Some(winner) => s"${winner.name} won"
      case None =>
        if (moreThan(6)) {
          if (second.score == first.score) "deuce"
          else if (hasAdvantage) s"advantage ${leader.name}"
        } else {
          s"${first.description}, ${second.description}"
        }
    }
  }


}

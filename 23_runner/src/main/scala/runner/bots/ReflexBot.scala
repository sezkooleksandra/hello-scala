package runner.bots

import runner.*

class ReflexBot(seed: Long = 202L) extends CyberBot:

  override def name: String = "ReflexBot"

  override def decide(observation: Observation): Action =
    val lane = observation.hero.lane
    observation.upcoming match
      case Nil => Action.KeepRunning
      case next :: later =>
        val future = depthTable(later)
        val best = candidates(lane).maxBy(action => score(lane, action, next, future))
        guarded(lane, best)

  private def guarded(lane: Lane, action: Action): Action =
    action match
      case Action.MoveLeft  if lane.left.isEmpty  => Action.KeepRunning
      case Action.MoveRight if lane.right.isEmpty => Action.KeepRunning
      case other                                  => other


  private def candidates(lane: Lane): List[Action] =
    List(Action.KeepRunning, Action.Duck, Action.Jump) ++
      lane.left.map(_ => Action.MoveLeft).toList ++
      lane.right.map(_ => Action.MoveRight).toList

  private def outcome(lane: Lane, action: Action): (Lane, Stance) =
    action match
      case Action.MoveLeft    => (lane.left.getOrElse(lane), Stance.Running)
      case Action.MoveRight   => (lane.right.getOrElse(lane), Stance.Running)
      case Action.Jump        => (lane, Stance.Jumping)
      case Action.Duck        => (lane, Stance.Ducking)
      case Action.KeepRunning => (lane, Stance.Running)

  // Правила виживання (як у рушії):
  // Running: немає Low і Mid; Jumping: немає High; Ducking: немає Low.
  private def survives(stance: Stance, lane: Lane, slice: TunnelSlice): Boolean =
    val low  = slice.hasObstacleAt(lane, Height.Low)
    val mid  = slice.hasObstacleAt(lane, Height.Mid)
    val high = slice.hasObstacleAt(lane, Height.High)
    stance match
      case Stance.Running => !low && !mid
      case Stance.Jumping => !high
      case Stance.Ducking => !low

  private def score(
      lane: Lane,
      action: Action,
      slice: TunnelSlice,
      future: Map[Lane, Int]
  ): Int =
    val (nextLane, stance) = outcome(lane, action)
    if survives(stance, nextLane, slice) then 1 + future(nextLane) else 0

  private def depthTable(slices: List[TunnelSlice]): Map[Lane, Int] =
    val lanes = Lane.values.toList
    val zero: Map[Lane, Int] = lanes.map(lane => lane -> 0).toMap
    slices.foldRight(zero) { (slice, next) =>
      lanes.map { lane =>
        lane -> candidates(lane).map(action => score(lane, action, slice, next)).max
      }.toMap
    }
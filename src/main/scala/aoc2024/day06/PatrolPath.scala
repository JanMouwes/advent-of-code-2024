package aoc2024.day06

type WalkedStep = (Coordinate, Direction)

trait PatrolPath {
  def steps: Iterable[WalkedStep]

  def contains(cell: Coordinate): Boolean

  def containsStep(step: WalkedStep): Boolean

  def overlaps(other: PatrolPath): Boolean

  def extend(to: WalkedStep): PatrolPath

  def loops: Boolean
}


class StepPatrolPath(val steps: Iterable[WalkedStep]) extends PatrolPath {
  private val stepSet = steps.toSet
  private val cellSet = steps.map(_._1).toSet

  override def contains(cell: Coordinate): Boolean = cellSet.contains(cell)

  override def containsStep(step: WalkedStep): Boolean = stepSet.contains(step)

  override def overlaps(other: PatrolPath): Boolean = {
    other match {
      case path: SegmentedPatrolPath => path.segments.exists(seg => this.stepSet.exists((coordinate, direction) => {
        seg.direction == direction && seg.contains(coordinate)
      }))
      case path: StepPatrolPath => this.stepSet.intersect(path.stepSet).nonEmpty
    }
  }

  override def extend(to: WalkedStep): StepPatrolPath = StepPatrolPath(steps.concat(Iterable.single(to)))

  override def loops: Boolean = {
    val (loc, dir) = steps.last
    this.containsStep(loc + dir.unitVector, dir)
  }
}

object StepPatrolPath {
  def empty = SegmentedPatrolPath(Iterable.empty)
}

class SegmentedPatrolPath(val segments: Iterable[Segment]) extends PatrolPath {
  override def steps: Iterable[WalkedStep] = this.segments.flatMap(seg => seg.steps)

  override def contains(cell: Coordinate): Boolean = segments.exists(seg => seg.contains(cell))

  override def containsStep(step: WalkedStep): Boolean = segments.exists(seg => seg.direction == step._2 && seg.contains(step._1))

  override def overlaps(other: PatrolPath): Boolean = {
    other match {
      case that: SegmentedPatrolPath => this.overlapsAnySegment(that.segments)
      case that: StepPatrolPath => that.overlaps(this)
    }
  }

  private def overlapsAnySegment(that: Iterable[Segment]): Boolean = {
    that.exists(seg => this.segments.exists(_.overlaps(seg)))
  }

  def extend(to: WalkedStep): SegmentedPatrolPath = {
    if (this.segments.isEmpty) return SegmentedPatrolPath(Iterable.single(Segment.fromWalkedStep(to)))

    val last = this.segments.last

    val segments =
      if last.direction == to._2
      then this.segments.init ++ Iterable.single(last.extend(to._1))
      else this.segments ++ Iterable.single(Segment.fromWalkedStep(to))

    SegmentedPatrolPath(segments)
  }

  override def loops: Boolean = {
    val (loc, dir) = (this.segments.last.to, this.segments.last.direction)
    this.containsStep(loc + dir.unitVector, dir)
  }
}

object SegmentedPatrolPath {
  def fromCells(steps: Seq[WalkedStep]): SegmentedPatrolPath = {
    SegmentedPatrolPath(Segment.fromWalkedSteps(steps))
  }

  def empty: SegmentedPatrolPath = SegmentedPatrolPath(Seq())

  def start(walkedStep: WalkedStep): SegmentedPatrolPath = fromCells(Seq(walkedStep))
}

private class Segment(val from: Coordinate, val to: Coordinate, val direction: Direction) {
  private val isValid = from.x == to.x || from.y == to.y
  if (!isValid) throw new IllegalArgumentException(s"from (${from.toString}) and to (${to.toString}) not grid-aligned")

  /**
   * Checks if given walked step is anywhere in the segment, including end points
   */
  def contains(needle: Coordinate): Boolean = {

    if this.isHorizontal
    then needle.y == this.from.y
      && Math.min(this.from.x, this.to.x) <= needle.x
      && needle.x <= Math.max(this.from.x, this.to.x)
    else needle.x == this.from.x
      && Math.min(this.from.y, this.to.y) <= needle.y
      && needle.y <= Math.max(this.from.y, this.to.y)
  }

  def overlaps(that: Segment): Boolean = {
    if this.direction != that.direction then return false
    this.contains(that.from) || this.contains(that.to) || that.contains(this.from) || that.contains(this.to)
  }

  private def isHorizontal: Boolean = this.direction == Direction.West || this.direction == Direction.East

  def extend(to: Coordinate): Segment = {
    val seg = Segment(this.from, to, this.direction)
    if (!seg.contains(this.to)) {
      throw new IllegalArgumentException(s"${this.toString} could not be extended to ${to}")
    }
    seg
  }

  def steps: Seq[WalkedStep] = {
    if this.isHorizontal
    then for i <- this.from.x to this.to.x by this.direction.unitVector.x
    yield (Coordinate(i, this.from.y), this.direction)
    else for i <- this.from.y to this.to.y by this.direction.unitVector.y
    yield (Coordinate(this.from.x, i), this.direction)
  }

  override def toString: String = s"Segment(${this.from}, ${this.to}, ${direction})"
}

private object Segment {
  def fromWalkedSteps(steps: Seq[WalkedStep]): Seq[Segment] = {
    if (steps.isEmpty) return Seq()

    steps.tail.foldLeft((Segment.fromWalkedStep(steps.head), List[Segment]())) {
      (state, step) => {
        val (current, segments) = state
        val (stepLocation, stepDirection) = step
        val predicted = current.to + current.direction.unitVector
        if current.direction == stepDirection && predicted == stepLocation
        then (Segment(current.from, stepLocation, current.direction), segments)
        else (Segment.fromWalkedStep(step), segments :+ current)
      }
    }._2
  }

  def fromWalkedStep(step: WalkedStep): Segment = Segment(step._1, step._1, step._2)
}
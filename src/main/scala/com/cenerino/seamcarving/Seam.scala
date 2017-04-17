package com.cenerino.seamcarving

import scala.math.abs

sealed abstract class Seam(private val pixels: IndexedSeq[Pos]) extends Seq[Pos] {

  private def areEntriesAdjacent =
    (pixels sliding 2) forall { case Seq(current, next) => isAdjacent(current, next) }

  require(pixels.length > 0, "Cannot be empty")
  require(pixels.length == 1 || areEntriesAdjacent, "One or more adjacent entries differ by more than 1 pixel")

  protected def isAdjacent(current: Pos, next: Pos): Boolean

  override def length: Int = pixels.length

  override def apply(idx: Int): Pos = pixels(idx)

  override def iterator: Iterator[Pos] = pixels.iterator
}

case class HorizontalSeam(pixels: Seq[Pos]) extends Seam(pixels.toVector) {
  override protected def isAdjacent(current: Pos, next: Pos) = {
    val (curCol, curRow) = current
    val (nextCol, nextRow) = next
    nextCol - curCol == 1 && abs(nextRow - curRow) <= 1
  }
}

case class VerticalSeam(pixels: Seq[Pos]) extends Seam(pixels.toVector) {
  override protected def isAdjacent(current: Pos, next: Pos) = {
    val (curCol, curRow) = current
    val (nextCol, nextRow) = next
    nextRow - curRow == 1 && abs(nextCol - curCol) <= 1
  }
}
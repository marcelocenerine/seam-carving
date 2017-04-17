package com.cenerino.seamcarving

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.math.abs

class Seam private(private val pixels: IndexedSeq[Pos]) extends Seq[Pos] {

  def allEntriesAdjacentToEachOther = {
    def isAdjacent(predecessor: Pos, current: Pos) = {
      val (preCol, preRow) = predecessor
      val (curCol, curRow) = current
      abs(curCol - preCol) <= 1 && abs(curRow - preRow) <= 1
    }

    if (pixels.length > 1)
      (pixels sliding 2).forall { case Seq(predecessor, current) => isAdjacent(predecessor, current) }
    else true
  }

  require(pixels.length > 0, "Cannot be empty")
  require(allEntriesAdjacentToEachOther, "One or more adjacent entries differ by more than 1 pixel")

  def transpose: Seam = new Seam(pixels map (_.swap))

  def isVertical: Boolean = {
    val (_, firstRow) = head
    val (_, lastRow) = last
    firstRow == 0 && lastRow == pixels.size - 1
  }

  def isHorizontal: Boolean = !isVertical

  override def length: Int = pixels.length

  override def apply(idx: Int): Pos = pixels.apply(idx)

  override def iterator: Iterator[Pos] = pixels.iterator

  override def newBuilder: mutable.Builder[Pos, Seam] = Seam.newBuilder
}

object Seam {

  def from(pixels: Seq[Pos]): Seam = new Seam(pixels.toVector)

  def newBuilder = new ArrayBuffer[Pos] mapResult(from)

  implicit def canBuildFrom: CanBuildFrom[Seam, Pos, Seam] = new CanBuildFrom[Seam, Pos, Seam] {
    def apply(): mutable.Builder[Pos, Seam] = newBuilder
    def apply(from: Seam): mutable.Builder[Pos, Seam] = newBuilder
  }
}
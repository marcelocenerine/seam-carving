package com.cenerino.seamcarving

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.collection.{SeqLike, mutable}
import scala.math.abs

class Seam private (private val pixels: Seq[Pos]) extends Seq[Pos] with SeqLike[Pos, Seam]{

  def allEntriesAdjacentToEachOther = {
    def isAdjacent(predecessor: Pos, current: Pos) = {
      val (preCol, preRow) = predecessor
      val (curCol, curRow) = current
      abs(curCol - preCol) <= 1 && abs(curRow - preRow) <= 1
    }

    (pixels zip (pixels tail)) forall { case (predecessor, current) => isAdjacent(predecessor, current) }
  }

  require(allEntriesAdjacentToEachOther, "One or more adjacent entries differ by more than 1 pixel")

  def transpose: Seam = map (_.swap)

  override def length: Int = pixels.length

  override def apply(idx: Int): Pos = pixels.apply(idx)

  override def iterator: Iterator[Pos] = pixels.iterator

  override def newBuilder: mutable.Builder[Pos, Seam] = Seam.newBuilder
}

object Seam {

  def from(pixels: Seq[Pos]): Seam = new Seam(pixels)

  def newBuilder = new ArrayBuffer[Pos] mapResult(from)

  implicit def canBuildFrom: CanBuildFrom[Seam, Pos, Seam] = new CanBuildFrom[Seam, Pos, Seam] {
      def apply(): mutable.Builder[Pos, Seam] = newBuilder
      def apply(from: Seam): mutable.Builder[Pos, Seam] = newBuilder
  }
}
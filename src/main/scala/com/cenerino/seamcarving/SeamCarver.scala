package com.cenerino.seamcarving

import scala.collection.mutable
import scala.math._

object SeamCarver {

  def nextHorizontalSeam(image: Image): Seam = nextVerticalSeam(image transpose) transpose

  def nextVerticalSeam(image: Image): Seam = {
    val width = image.width
    val height = image.height
    val energy = EnergyFunction.dualGradient(image)

    val distTo = Array.fill(width, height)(Double.PositiveInfinity)
    val edgeTo = Array.ofDim[Pos](width, height)
    val visited = Array.ofDim[Boolean](width, height)
    val queue = mutable.Queue[Pos]()

    // populates first row
    for (c <- 0 until width; r = 0) {
      distTo(c)(r) = energy(c, r)
      queue += ((c, r))
    }

    while (queue.nonEmpty) {
      val pos = queue.dequeue
      val (col, row) = pos
      // relax
      val dist = distTo(col)(row)
      val e = energy(col, row)

      for ((adjCol, adjRow) <- verticallyAdjacentPixels(pos, image)) {
        if (e + dist < distTo(adjCol)(adjRow)) {
          distTo(adjCol)(adjRow) = e + dist
          edgeTo(adjCol)(adjRow) = pos
        }

        if (!visited(adjCol)(adjRow)) {
          queue += ((adjCol, adjRow))
          visited(adjCol)(adjRow) = true
        }
      }
    }

    // find min seam
    val lastRowInSeam = height - 1
    val distancesAtLastRow = for (col <- 0 until width; dist = distTo(col)(lastRowInSeam)) yield (col, dist)
    val (lastColInSeam, _) = distancesAtLastRow minBy (_._2)
    // walks backwards until reaching the start of path
    lazy val seam: Stream[Pos] = Stream.cons((lastColInSeam, lastRowInSeam), seam.map { case (c, r) => edgeTo(c)(r) })
    Seam.from((seam take height) reverse)
  }

  private def verticallyAdjacentPixels(pos: Pos, image: Image): Seq[Pos] = {
    val result = mutable.ListBuffer[Pos]()
    val (c, r) = pos

    if (r < image.height - 1) {
      result += ((c, r + 1))
      if (c > 0) result += ((c - 1, r + 1))
      if (c < image.width - 1) result += ((c + 1, r + 1))
    }

    result
  }
}
package com.cenerino.seamcarving

import scala.collection.mutable

private abstract class SeamCarver {
  val image: Image
  val seamLength: Int
  val width = image.width
  val height = image.height
  val energy = EnergyFunction.dualGradient(image)

  def startingPixels: Seq[Pos]
  def endingPixels: Seq[Pos]
  def adjacentPixels(pos: Pos): Seq[Pos]

  def minimumEnergyPath: Seq[Pos] = {
    val distTo = Array.fill(width, height)(Double.PositiveInfinity)
    val edgeTo = Array.ofDim[Pos](width, height)
    val visited = Array.ofDim[Boolean](width, height)
    val queue = mutable.Queue[Pos]()

    // populates starting pixels
    for (px <- startingPixels) {
      distTo(px._1)(px._2) = energy(px)
      queue += px
    }

    while (queue.nonEmpty) {
      val pos = queue.dequeue
      val (col, row) = pos
      // relax
      val dist = distTo(col)(row)
      val e = energy(col, row)

      for ((adjCol, adjRow) <- adjacentPixels(pos)) {
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

    val (endingColumn, endingRow) = endingPixels minBy { case (c, r) => distTo(c)(r) }
    // walks backwards until reaching the start of path
    lazy val path: Stream[Pos] = Stream.cons((endingColumn, endingRow), path.map { case (c, r) => edgeTo(c)(r) })
    (path take seamLength) reverse
  }
}

private class HorizontalCarver(val image: Image) extends SeamCarver {
  override val seamLength = width
  override def startingPixels = (0 until height) map (r => (0, r))
  override def endingPixels = (0 until height) map (r => (width - 1, r))

  override def adjacentPixels(pos: Pos) =  {
    val result = mutable.ListBuffer[Pos]()
    val (c, r) = pos

    if (c < width - 1) {
      result += ((c + 1, r))
      if (r > 0) result += ((c + 1, r - 1))
      if (r < height - 1) result += ((c + 1, r + 1))
    }

    result
  }

  def next: Seam = new HorizontalSeam(minimumEnergyPath)
}

private class VerticalCarver(val image: Image) extends SeamCarver {
  override val seamLength = height
  override def startingPixels = (0 until width) map (c => (c, 0))
  override def endingPixels = (0 until width) map (c => (c, height - 1))

  override def adjacentPixels(pos: Pos) =  {
    val result = mutable.ListBuffer[Pos]()
    val (c, r) = pos

    if (r < height - 1) {
      result += ((c, r + 1))
      if (c > 0) result += ((c - 1, r + 1))
      if (c < width - 1) result += ((c + 1, r + 1))
    }

    result
  }

  def next: Seam = new VerticalSeam(minimumEnergyPath)
}

object SeamCarver {
  def nextHorizontalSeam(image: Image) = new HorizontalCarver(image).next
  def nextVerticalSeam(image: Image) = new VerticalCarver(image).next
}
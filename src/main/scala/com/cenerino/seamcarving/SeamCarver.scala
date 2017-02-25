package com.cenerino.seamcarving

import java.awt.Color
import java.awt.Color._

import scala.collection.mutable
import scala.math._

object SeamCarver {

  // TODO move it somewhere else
  def energyPicture(image: Image, showVerticalSeam: Boolean = false, showHorizontalSeam: Boolean = false): Image = {
    val pixels = Array.ofDim[Int](image.width, image.height)
    val energy = EnergyFunction.dualGradient(image)
    val energyMatrix = (0 until image.width).map(c => (0 until image.height).map(r => energy(c, r)).toArray).toArray

    // maximum gray scale value (ignoring border pixels)
    val maxVal = (for (col <- 1 until image.width - 1; row <- 1 until image.height - 1) yield energyMatrix(col)(row)).max

    if (maxVal != 0) {
      for {col <- 0 until image.width; row <- 0 until image.height} {
        val normalized = min((energyMatrix(col)(row) / maxVal).toFloat, 1.0f)
        pixels(col)(row) = new Color(normalized, normalized, normalized).getRGB
      }
    }

    val drawSeam = (seam: Seam) => seam.foreach { case (c, r) => pixels(c)(r) = RED.getRGB }

    if (showVerticalSeam) drawSeam(nextVerticalSeam(image))
    if (showHorizontalSeam) drawSeam(nextHorizontalSeam(image))

    Image(pixels)
  }

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

    val lastRowInSeam = height - 1
    val lastColInSeam = (0 until width).map(c => (c, distTo(c)(lastRowInSeam))).reduceLeft((a, b) => if (b._2 > a._2) a else b)._1
    lazy val seam: Stream[Pos] = Stream.cons((lastColInSeam, lastRowInSeam), seam.map { case (c, r) => edgeTo(c)(r) })
    (seam take height) reverse
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

  def nextHorizontalSeam(image: Image): Seam = nextVerticalSeam(image transpose) map (_.swap)

  def removeVerticalSeam(seam: Seam, image: Image): Image = {
    require(image.width > 1, "Image cannot be vertically resized")
    require(seam.length == image.height, "Seam length does not match image height")
    validateSeam(seam, image)

    val newWidth = image.width - 1
    val pixels = Array.ofDim[Int](newWidth, image.height)

    for {
      (col, row) <- seam
      targetCol <- 0 until newWidth
      sourceCol = if (targetCol < col) targetCol else targetCol + 1
    } pixels(targetCol)(row) = image.rgb(sourceCol, row)

    Image(pixels)
  }

  def removeHorizontalSeam(seam: Seam, image: Image): Image = {
    require(image.height > 1, "Image cannot be horizontally resized")
    require(seam.length == image.width, "Seam length does not match image width")
    validateSeam(seam, image)

    val newHeight = image.height - 1
    val pixels = Array.ofDim[Int](image.width, newHeight)

    for {
      (col, row) <- seam
      targetRow <- 0 until newHeight
      sourceRow = if (targetRow < row) targetRow else targetRow + 1
    } pixels(col)(targetRow) = image.rgb(col, sourceRow)

    Image(pixels)
  }

  private def validateSeam(seam: Seam, image: Image): Unit = {
    def assertPixelsAreWithinBounds(pixel: Pos) = {
      val (col, row) = pixel
      if (col < 0 || col >= image.width || row < 0 || row >= image.height) 
        throw new IndexOutOfBoundsException("invalid index")
    }

    def areElementsInSeamAdjacentToEachOther = {
      def isAdjacent(predecessor: Pos, current: Pos) = {
        val (preCol, preRow) = predecessor
        val (curCol, curRow) = current
        abs(curCol - preCol) <= 1 && abs(curRow - preRow) <= 1
      }

      (seam zip (seam tail)) forall { case (pre, current) => isAdjacent(pre, current) }
    }

    seam foreach assertPixelsAreWithinBounds
    require(areElementsInSeamAdjacentToEachOther, "One or more adjacent entries differ by more than 1 pixel")
  }
}
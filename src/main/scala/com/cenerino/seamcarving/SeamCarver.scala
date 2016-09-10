package com.cenerino.seamcarving

import scala.collection.mutable
import scala.math._
import SeamCarver._

class SeamCarver private(var width: Int, var height: Int) {

  private val pixels = Array.ofDim[Int](width, height)

  type Pos = (Int, Int)
  type Seam = Seq[Pos]

  def this(pic: Picture) {
    this(pic.width, pic.height)

    for {
      c <- 0 until width
      r <- 0 until height
    } pixels(c)(r) = pic.rgb(c, r)
  }

  def picture: Picture = {
    val pic = Picture(width, height)

    for {
      c <- 0 until width
      r <- 0 until height
    } pic.rgb(c, r, pixels(c)(r))

    pic
  }

  def energy(pixel: Pos): Double = {
    assertIsWithinBounds(pixel)
    val (c, r) = pixel

    if (c == 0 || c == width - 1 || r == 0 || r == height - 1) BorderEnergy
    else {
      val deltaX = delta(pixels(c - 1)(r), pixels(c + 1)(r))
      val deltaY = delta(pixels(c)(r - 1), pixels(c)(r + 1))
      sqrt(deltaX + deltaY)
    }
  }

  private def delta(rgb1: Int, rgb2: Int): Double = {
    val r = red(rgb1) - red(rgb2)
    val g = green(rgb1) - green(rgb2)
    val b = blue(rgb1) - blue(rgb2)
    pow(r, 2) + pow(g, 2) + pow(b, 2)
  }

  private def red(rgb: Int): Int = (rgb >> 16) & 0xFF

  private def green(rgb: Int): Int = (rgb >> 8) & 0xFF

  private def blue(rgb: Int): Int = (rgb >> 0) & 0xFF

  def findHorizontalSeam: Seam = {
    val distTo = Array.fill(width, height)(Double.PositiveInfinity)
    val edgeTo = Array.ofDim[Pos](width, height)
    val visited = Array.ofDim[Boolean](width, height)
    val queue = mutable.Queue[Pos]()

    // populates first column
    for (r <- 0 until height; c = 0) {
      distTo(c)(r) = energy(c, r)
      queue += ((c, r))
    }

    while (queue.nonEmpty) {
      val (col, row) = queue.dequeue
      // relax
      val dist = distTo(col)(row)
      val e = energy(col, row)

      for ((adjCol, adjRow) <- horizontallyAdjacentPixels(col, row)) {
        if (e + dist < distTo(adjCol)(adjRow)) {
          distTo(adjCol)(adjRow) = e + dist
          edgeTo(adjCol)(adjRow) = (col, row)
        }

        if (!visited(adjCol)(adjRow)) {
          queue += ((adjCol, adjRow))
          visited(adjCol)(adjRow) = true
        }
      }
    }

    val lastColInSeam = width - 1
    val lastRowInSeam = (0 until height).map(r => (r, distTo(lastColInSeam)(r))).reduceLeft((a, b) => if (b._2 > a._2) b else a)._1
    lazy val seam: Stream[Pos] = Stream.cons((lastColInSeam, lastRowInSeam), seam.map { case (c, r) => edgeTo(c)(r) })
    (seam take width) reverse
  }

  private def horizontallyAdjacentPixels(pixel: Pos): Seq[Pos] = {
    val result = mutable.ListBuffer[Pos]()
    val (c, r) = pixel

    if (c < width - 1) {
      result += ((c + 1, r))
      if (r > 0) result += ((c + 1, r - 1))
      if (r < height - 1) result += ((c + 1, r + 1))
    }

    result
  }

  def findVerticalSeam: Seam = {
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
      val (col, row) = queue.dequeue
      // relax
      val dist = distTo(col)(row)
      val e = energy(col, row)

      for ((adjCol, adjRow) <- verticallyAdjacentPixels(col, row)) {
        if (e + dist < distTo(adjCol)(adjRow)) {
          distTo(adjCol)(adjRow) = e + dist
          edgeTo(adjCol)(adjRow) = (col, row)
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

  private def verticallyAdjacentPixels(pixel: Pos): Seq[Pos] = {
    val result = mutable.ListBuffer[Pos]()
    val (c, r) = pixel

    if (r < height - 1) {
      result += ((c, r + 1))
      if (c > 0) result += ((c - 1, r + 1))
      if (c < width - 1) result += ((c + 1, r + 1))
    }

    result
  }

  def removeSeam(seam: Seam): Unit = {
    seam.foreach { assertIsWithinBounds(_) }
    assertPixelsAreAdjacent(seam)

    if (seam.length == width) removeHorizontalSeam(seam)
    else if (seam.length == height) removeVerticalSeam(seam)
    else throw new IllegalArgumentException("Invalid seam length")
  }

  private def removeHorizontalSeam(seam: Seam): Unit = {
    require(height > 1, "Image cannot be horizontally resized")

    for {
      (col, row) <- seam
      r <- row until (height - 1)
    } pixels(col)(r) = pixels(col)(r + 1)

    height -= 1
  }

  private def removeVerticalSeam(seam: Seam): Unit = {
    require(width > 1, "Image cannot be vertically resized")

    for {
      (col, row) <- seam
      c <- col until (width - 1)
    } pixels(c)(row) = pixels(c + 1)(row)

    width -= 1
  }

  private def assertPixelsAreAdjacent(seam: Seam): Unit = {
    require(((seam) zip (seam tail))
        .forall { case (pre, current) => isValidAdjacency(pre, current) },
      "One or more adjacent entries differ by more than 1 pixel")
  }

  private def assertIsWithinBounds(pixel: Pos): Unit = {
    val (c, r) = pixel
    if (c < 0 || c >= width || r < 0 || r >= height) throw new IndexOutOfBoundsException("invalid index")
  }

  private def isValidAdjacency(predecessor: Pos, current: Pos): Boolean = {
    val (preCol, preRow) = predecessor
    val (curCol, curRow) = current
    abs(curCol - preCol) <= 1 && abs(curRow - preRow) <= 1
  }
}

object SeamCarver {
  val BorderEnergy = 1000
  def apply(pic: Picture): SeamCarver = new SeamCarver(pic)
}

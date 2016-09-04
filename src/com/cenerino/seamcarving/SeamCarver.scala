package com.cenerino.seamcarving

import scala.collection.mutable
import scala.math._

class SeamCarver private(var width: Int, var height: Int) {

  private val pixels = Array.ofDim[Int](width, height)

  type Pos = (Int, Int)
  type Seam = Seq[Pos]

  def this(pic: Picture) {
    this(pic.width, pic.height)

    for {
      c <- 0 until width
      r <- 0 until height
    } pixels(c)(r) = pic.getRGB(c, r)
  }

  def picture: Picture = {
    val pic = Picture(width, height)

    for {
      c <- 0 until width
      r <- 0 until height
    } pic.setRGB(c, r, pixels(c)(r))

    pic
  }

  def energy(pixel: Pos): Double = {
    assertIsWithinBounds(pixel)
    val (c, r) = pixel

    if (c == 0 || c == width - 1 || r == 0 || r == height - 1) 1000
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
    pow(r, 2) + Math.pow(g, 2) + Math.pow(b, 2)
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

      for ((adjCol, adjRow) <- adjHorizontal(col, row)) {
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

    // find min horizontal seam
    val lastCol = width - 1
    val rowOfLastPixelInSeam = (0 until height).map(r => (r, distTo(lastCol)(r))).reduceLeft((a, b) => if (a._2 <= b._2) a else b)._1
    val lastPixelInSeam: Pos = (lastCol, rowOfLastPixelInSeam)
    lazy val seam: Stream[Pos] = Stream.cons(lastPixelInSeam, seam.map { case (c, r) => edgeTo(c)(r) })
    (seam take width) reverse
  }

  private def adjHorizontal(pixel: Pos): Seq[Pos] = {
    val result = mutable.ListBuffer[Pos]()
    val (c, r) = pixel

    if (c < width - 1) {
      result += ((c + 1, r))
      if (r > 0) result += ((c + 1, r - 1))
      if (r < height - 1) result += ((c + 1, r + 1))
    }

    result
  }

  def removeHorizontalSeam(seam: Seam): Unit = {
    require(height > 1, "Image cannot be horizontally resized")
    require(seam.length == width, "Seam length does not match image width")
    assertSeamIsValid(seam)

    seam.foreach { case (c, r) => pixels(c)(r) = pixels(c)(r + 1) }
    height -= height
  }

  def findVerticalSeam: Seam = ???

  def removeVerticalSeam(seam: Seam): Unit = {
    require(width > 1, "Image cannot be vertically resized")
    require(seam.length == height, "Seam length does not match image height")
    assertSeamIsValid(seam)

    seam.foreach { case (c, r) => pixels(c)(r) = pixels(c + 1)(r) }
    width -= width
  }

  private def assertSeamIsValid(seam: Seam): Unit = {
    seam.foreach(pixel => assertIsWithinBounds(pixel))
    require(((seam) zip (seam tail)).forall { case (pre, current) => isValidAdjacency(pre, current) },
      "One or more adjacent entries differ by more than 1 pixel")
  }

  private def assertIsWithinBounds(pixel: Pos): Unit = pixel match {
    case (c, r) => if (c < 0 || c >= width || r < 0 || r >= height) throw new IndexOutOfBoundsException("invalid index")
  }

  private def isValidAdjacency(predecessor: Pos, current: Pos): Boolean = {
    val (preCol, preRow) = predecessor
    val (curCol, curRow) = current
    abs(curCol - preCol) <= 1 && abs(curRow - preRow) <= 1
  }
}

object SeamCarver {
  def apply(pic: Picture): SeamCarver = new SeamCarver(pic)
}

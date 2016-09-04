package com.cenerino.seamcarving

import scala.collection.mutable
import scala.math._

class SeamCarver private(var width: Int, var height: Int) {

  private val pixels = Array.ofDim[Int](width * height)
  private val originalWidth = width

  type Pos = (Int, Int)
  type Seam = Seq[Pos]

  def this(pic: Picture) {
    this(pic.width, pic.height)

    for {
      c <- 0 until width
      r <- 0 until height
    } pixels(idx(c, r)) = pic.getRGB(c, r)
  }

  private def idx(pixel: Pos): Int = pixel match {
    case (c, r) => r * originalWidth + c
  }

  private def row(idx: Int): Int = idx / originalWidth

  private def col(idx: Int): Int = idx % originalWidth

  def picture: Picture = {
    val pic = Picture(width, height)

    for {
      c <- 0 until width
      r <- 0 until height
    } pic.setRGB(c, r, pixels(idx(c, r)))

    pic
  }

  def energy(pixel: Pos): Double = {
    assertIsWithinBounds(pixel)
    val (c, r) = pixel

    if (c == 0 || c == width - 1 || r == 0 || r == height - 1) 1000
    else {
      val deltaX = delta(pixels(idx(c - 1, r)), pixels(idx(c + 1, r)))
      val deltaY = delta(pixels(idx(c, r - 1)), pixels(idx(c, r + 1)))
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
    val n = height * originalWidth
    val distTo = Array.fill(n)(Double.PositiveInfinity)
    val edgeTo = Array.ofDim[Int](n)
    val visited = Array.ofDim[Boolean](n)
    val queue = mutable.Queue[Int]()

    for (r <- 0 until height) {
      val pixel = idx(0, r)
      distTo(pixel) = energy(0, r)
      queue += pixel
    }

    while (queue.nonEmpty) {
      val from = queue.dequeue
      // relax
      val dist = distTo(from)

      for (adj <- adjHorizontal(from)) {
        val r = row(adj)
        val c = col(adj)
        val e = energy(c, r)

        if (e + dist < distTo(adj)) {
          distTo(adj) = e + dist
          edgeTo(adj) = from
        }

        if (!visited(adj)) {
          queue += adj
          visited(adj) = true
        }
      }
    }

    // find min horizontal seam
    val lastCol = width - 1
    // TODO simplify it
    val min = Stream.range(0, height)
      .map(row => idx(lastCol, row))
      .map(idx => (idx, distTo(idx)))
      .reduceLeft((a, b) => if (a._2 <= b._2) a else b)
      ._1
    // builds the seam moving backwards from min
    lazy val seam: Stream[Int] = Stream.cons(min, seam.map(e => edgeTo(e)))
    // combine the column indexes with the row indexes
    (0 until width) zip (seam.map(idx => row(idx)))
  }

  private def adjHorizontal(pixel: Int): Seq[Integer] = {
    val result = mutable.ListBuffer[Integer]()
    val c = col(pixel)
    val r = row(pixel)

    if (c < width - 1) {
      result += idx(c + 1, r)
      if (r > 0) result += idx(c + 1, r - 1)
      if (r < height - 1) result += idx(c + 1, r + 1)
    }

    result
  }

  def removeHorizontalSeam(seam: Seam): Unit = {
    require(height > 1, "Image cannot be horizontally resized")
    require(seam.length == width, "Seam length does not match image width")
    assertSeamIsValid(seam)

    seam.foreach { case (c, r) => pixels(idx(c, r)) = pixels(idx(c, r + 1)) }
    height -= height
  }

  def findVerticalSeam: Seam = ???

  def removeVerticalSeam(seam: Seam): Unit = {
    require(width > 1, "Image cannot be vertically resized")
    require(seam.length == height, "Seam length does not match image height")
    assertSeamIsValid(seam)

    seam.foreach { case (c, r) => pixels(idx(c, r)) = pixels(idx(c + 1, r)) }
    width -= width
  }

  private def assertSeamIsValid(seam: Seam): Unit = {
    seam.foreach(pixel => assertIsWithinBounds(pixel))
    require(((seam tail) zip (seam)).forall { case (cur, pre) => isValidAdjacency(cur, pre) },
      "One or more adjacent entries differ by more than 1 pixel")
  }

  private def assertIsWithinBounds(pixel: Pos): Unit = pixel match {
    case (c, r) => if (c < 0 || c >= width || r < 0 || r >= height) throw new IndexOutOfBoundsException("invalid index")
  }

  private def isValidAdjacency(current: Pos, predecessor: Pos): Boolean = {
    val (preCol, preRow) = predecessor
    val (curCol, curRow) = current
    abs(curCol - preCol) <= 1 && abs(curRow - preRow) <= 1
  }
}

object SeamCarver {
  def apply(pic: Picture): SeamCarver = new SeamCarver(pic)
}

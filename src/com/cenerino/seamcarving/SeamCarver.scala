package com.cenerino.seamcarving

import java.awt.image.BufferedImage
import java.util.Arrays

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class SeamCarver private(val width: Int, val height: Int) {

  private val pixels = Array.ofDim[Int](width * height)
  private val originalWidth = width

  def this(image: BufferedImage) {
    this(image.getWidth, image.getHeight)
  }

  private def copyPixels(image: BufferedImage): Unit =
    for (c <- 0 to width; r <- 0 to height)
      pixels(idx(c, r)) = image.getRGB(c, r)

  private def idx(c: Int, r: Int): Int = r * originalWidth + c

  private def row(idx: Int): Int = idx / originalWidth

  private def col(idx: Int): Int = idx % originalWidth

  def image: BufferedImage = {
    val image = new BufferedImage(width, height, 1)

    for (c <- 0 to width; r <- 0 to height)
      image.setRGB(c, r, pixels(idx(c, r)))

    image
  }

  def energy(c: Int, r: Int): Double = {
    if (c < 0 || c >= width) throw new IndexOutOfBoundsException("invalid column index")
    if (r < 0 || r >= height) throw new IndexOutOfBoundsException("invalid row index")

    calcEnergy(c, r)
  }

  private def calcEnergy(c: Int, r: Int): Double =
    if (c == 0 || c == width - 1 || r == 0 || r == height - 1)
      1000
    else {
      val deltaX = calcDelta(pixels(idx(c - 1, r)), pixels(idx(c + 1, r)))
      val deltaY = calcDelta(pixels(idx(c, r - 1)), pixels(idx(c, r + 1)))
      Math.sqrt(deltaX + deltaY)
    }

  private def calcDelta(rgb1: Int, rgb2: Int): Double = {
    val r = red(rgb1) - red(rgb2)
    val g = green(rgb1) - green(rgb2)
    val b = blue(rgb1) - blue(rgb2)
    Math.pow(r, 2) + Math.pow(g, 2) + Math.pow(b, 2)
  }

  private def red(rgb: Int): Int = (rgb >> 16) & 0xFF

  private def green(rgb: Int): Int = (rgb >> 8) & 0xFF

  private def blue(rgb: Int): Int = (rgb >> 0) & 0xFF

  def findHorizontalSeam: Array[Int] = {
    val n = height * originalWidth
    val distTo = Array.fill(n)(Double.PositiveInfinity)
    val edgeTo = Array.ofDim[Int](n)
    val visited = Array.ofDim[Boolean](n)
    val queue = mutable.Queue[Int]()

    for (r <- 0 to height) {
      val pixel = idx(0, r)
      distTo(pixel) = calcEnergy(0, r)
      queue += pixel
    }

    while (queue.nonEmpty) {
      val from = queue.dequeue
      // relax
      val dist = distTo(from)

      for (adj <- adjHorizontal(from)) {
        val r = row(adj)
        val c = col(adj)
        val e = calcEnergy(c, r)

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
    val min = Stream.range(0, height)
        .map(_ => idx(lastCol, _))
        .min

    // build seam
    val seam = new Array[Int](width)

//    for {
//      idx <- (0 to width) reverse
//      e  edgeTo(e)
//    }
//
//    for (int idx = width - 1, e = min; idx >= 0; e = edgeTo[e], idx--) {
//      seam[idx] = row(e);
//    }

    seam
  }

  private def adjHorizontal(pixel: Int): Seq[Integer] = {
    val result = ListBuffer[Integer]()
    val c = col(pixel)
    val r = row(pixel)

    if (c < width - 1) {
      result += idx(c + 1, r)
      if (r > 0) result += idx(c + 1,r - 1)
      if (r < height - 1) result += idx(c + 1,r + 1)
    }

    result
  }
}

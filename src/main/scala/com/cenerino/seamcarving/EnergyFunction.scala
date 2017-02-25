package com.cenerino.seamcarving

import java.awt.Color

import scala.math._

trait EnergyFunction extends (Pos => Double) {
  def transformed: Image
}

object EnergyFunction {
  def dualGradient(image: Image): EnergyFunction = new DualGradient(image)
}

private class DualGradient(val image: Image) extends EnergyFunction {

  private val BorderEnergy = 1000

  def apply(pos: Pos): Double = {
    val (col, row) = pos

    require(col >= 0 && col < image.width)
    require(row >= 0 && row < image.height)

    if (isAtBorder(col, row, image)) BorderEnergy
    else {
      val deltaX = delta(image.rgb(col - 1, row), image.rgb(col + 1, row))
      val deltaY = delta(image.rgb(col, row - 1), image.rgb(col, row + 1))
      sqrt(deltaX + deltaY)
    }
  }

  private def isAtBorder(col: Int, row: Int, image: Image): Boolean =
    (col == 0 || col == image.width - 1) || (row == 0 || row == image.height - 1)

  private def delta(rgb1: Int, rgb2: Int): Double = {
    def red(rgb: Int): Int = (rgb >> 16) & 0xFF
    def green(rgb: Int): Int = (rgb >> 8) & 0xFF
    def blue(rgb: Int): Int = (rgb >> 0) & 0xFF

    val r = red(rgb1) - red(rgb2)
    val g = green(rgb1) - green(rgb2)
    val b = blue(rgb1) - blue(rgb2)
    pow(r, 2) + pow(g, 2) + pow(b, 2)
  }

  def transformed: Image = {
    val (width, height) = (image.width, image.height)
    val pixels = Array.ofDim[Int](width, height)
    val energyMatrix = Array.tabulate[Double](width, height)(this(_, _))

    // maximum gray scale value (ignoring border pixels)
    val maxVal = (for (col <- 1 until (width - 1); row <- 1 until (height - 1)) yield energyMatrix(col)(row)).max

    if (maxVal != 0) {
      for (col <- 0 until width; row <- 0 until height) {
        val normalized = min((energyMatrix(col)(row) / maxVal).toFloat, 1.0f)
        pixels(col)(row) = new Color(normalized, normalized, normalized).getRGB
      }
    }

    Image(pixels)
  }
}

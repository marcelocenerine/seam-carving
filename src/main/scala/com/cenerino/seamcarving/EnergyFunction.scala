package com.cenerino.seamcarving

import scala.math._

trait EnergyFunction extends (Pos => Double)

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
}

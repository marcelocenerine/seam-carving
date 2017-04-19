package com.cenerino.seamcarving

import scala.math._

sealed trait EnergyFunction extends (Pos => Double) {
  def energyPicture: Image
}

object EnergyFunction {
  def dualGradient(image: Image): EnergyFunction = new DualGradient(image)
}

private class DualGradient(private val image: Image) extends EnergyFunction {

  private val BorderEnergy = 1000.0

  def apply(pos: Pos): Double = {
    require(image.isDefinedAt(pos), "invalid coordinates")
    val (col, row) = pos

    if (isAtBorder(col, row)) BorderEnergy
    else {
      val deltaX = delta(image(col - 1, row), image(col + 1, row))
      val deltaY = delta(image(col, row - 1), image(col, row + 1))
      sqrt(deltaX + deltaY)
    }
  }

  private def isAtBorder(col: Int, row: Int): Boolean =
    (col == 0 || col == image.width - 1) || (row == 0 || row == image.height - 1)

  private def delta(rgb1: RGB, rgb2: RGB): Double = {
    val r = red(rgb1) - red(rgb2)
    val g = green(rgb1) - green(rgb2)
    val b = blue(rgb1) - blue(rgb2)
    pow(r, 2) + pow(g, 2) + pow(b, 2)
  }

  def energyPicture: Image = {
    val (width, height) = (image.width, image.height)
    val output = Array.fill[RGB](width, height)(Black)
    val energyMatrix = Array.tabulate[Double](width, height)(this(_, _))

    // maximum gray scale value (ignoring border pixels)
    val maxVal = (for (col <- 1 until (width - 1); row <- 1 until (height - 1)) yield energyMatrix(col)(row)).max

    if (maxVal != 0) {
      for (col <- 0 until width; row <- 0 until height) {
        val normalized = min((energyMatrix(col)(row) / maxVal).toFloat, 1.0f)
        output(col)(row) = rgb(normalized, normalized, normalized)
      }
    }

    Image(output)
  }
}

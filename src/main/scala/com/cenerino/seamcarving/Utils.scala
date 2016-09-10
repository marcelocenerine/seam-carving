package com.cenerino.seamcarving

import java.awt.Color

import scala.math._

object Utils {

  def energyPicture(seamCarver: SeamCarver): Picture = {
    val grayValues = energyMatrix(seamCarver)
    val width = seamCarver.width
    val height = seamCarver.height
    val picture = Picture(width, height)

    // maximum gray scale value (ignoring border pixels)
    val maxVal = (for {col <- 1 until width - 1; row <- 1 until height - 1} yield grayValues(col)(row)).max

    if (maxVal != 0) {
      for {
        col <- 0 until width
        row <- 0 until height
      } {
        val normalized = min((grayValues(col)(row) / maxVal).toFloat, 1.0f)
        picture.rgb(col, row, new Color(normalized, normalized, normalized))
      }
    }

    picture
  }

  private def energyMatrix(seamCarver: SeamCarver): Array[Array[Double]] = {
    val energyMatrix = Array.ofDim[Double](seamCarver.width, seamCarver.height)

    for {
      col <- 0 until seamCarver.width
      row <- 0 until seamCarver.height
    } energyMatrix(col)(row) = seamCarver.energy(col, row)

    energyMatrix
  }
}

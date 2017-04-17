package com.cenerino.seamcarving

import org.scalatest.FunSuite

class SeamCarverSuite extends FunSuite {

  // TODO add finer grained unit tests
  test("should resize both horizontally and vertically") {
    val input = Image("/chameleon.png")
    var output = input
    val expectedOutput = Image("/chameleon_resized.png")

    for (row <- 1 to 100) {
      val seam = SeamCarver.nextHorizontalSeam(output)
      output = output.removeHorizontalSeam(seam)
    }

    for (col <- 1 to 100) {
      val seam = SeamCarver.nextVerticalSeam(output)
      output = output.removeVerticalSeam(seam)
    }


    assert(output === expectedOutput)
  }
}

package com.cenerino.seamcarving.demo

import com.cenerino.seamcarving._
import SeamCarver._

object ShowSeams extends App {

  require(args.length == 3, "Usage:\njava ShowSeams <image filename> <show vertical seam> <show horizontal seam>")

  val input = Image(args(0))
  val showVerticalSeam = "true" == args(1)
  val showHorizontalSeam = "true" == args(2)
  var output = EnergyFunction.dualGradient(input).energyPicture

  if (showHorizontalSeam) {
    val seam = nextHorizontalSeam(input)
    output = output.drawn(seam, Red)
  }

  if (showVerticalSeam) {
    val seam = nextVerticalSeam(input)
    output = output.drawn(seam, Red)
  }

  new Frame(title = "Input", image = input).show()
  new Frame(title = "Output", image = output).show()
}

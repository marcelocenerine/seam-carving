package com.cenerino.seamcarving.demo

import com.cenerino.seamcarving.{SeamCarver, Image}

object ShowSeams extends App {

  require(args.length == 3, "Usage:\njava ShowSeams <image filename> <show vertical seam> <show horizontal seam>")

  val input = Image(args(0))
  val showVerticalSeam = "true" == args(1)
  val showHorizontalSeam = "true" == args(2)
  val output = SeamCarver.energyPicture(input, showVerticalSeam, showHorizontalSeam)

  new Frame(title = "Input", image = input).show()
  new Frame(title = "Output", image = output).show()
}

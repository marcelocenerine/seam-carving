package com.cenerino.seamcarving.demo

import com.cenerino.seamcarving.{SeamCarver, Image}

object ShowSeams extends App {

  require(args.length == 3, "Usage:\njava ShowSeams <image filename> <show vertical seam> <show horizontal seam>")

  val inputPicture = Image(args(0))
  val showVerticalSeam = "true" == args(1)
  val showHorizontalSeam = "true" == args(2)
  val outputPicture = SeamCarver.energyPicture(inputPicture, showVerticalSeam, showHorizontalSeam)

  inputPicture display (title = "Input")
  outputPicture display (title = "Output")
}

package com.cenerino.seamcarving

object ShowSeams extends App {

  require(args.length == 3, "Usage:\njava ShowSeams <image filename> <show vertical seam> <show horizontal seam>")

  val inputPicture = Picture(args(0))
  val showVerticalSeam = "true" == args(1)
  val showHorizontalSeam = "true" == args(2)
  val seamCarver = SeamCarver(inputPicture)
  val outputPicture = seamCarver.energyPicture(showVerticalSeam, showHorizontalSeam)

  inputPicture display (title = "Input")
  outputPicture display (title = "Output")
}

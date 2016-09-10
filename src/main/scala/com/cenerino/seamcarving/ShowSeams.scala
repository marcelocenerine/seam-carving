package com.cenerino.seamcarving

object ShowSeams extends App {

  require(args.length == 1, "Usage:\njava ShowSeams <image filename>")

  val inputPicture = Picture(args(0))
  val seamCarver = SeamCarver(inputPicture)
  val energyPicture = Utils.energyPicture(seamCarver)
  val seams = seamCarver.findHorizontalSeam ++ seamCarver.findVerticalSeam

  val outputPicture = Utils.seamOverlay(energyPicture, seams)

  inputPicture display (title = "Input")
  outputPicture display (title = "Output")
}

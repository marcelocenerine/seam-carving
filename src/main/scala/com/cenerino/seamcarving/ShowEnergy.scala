package com.cenerino.seamcarving

object ShowEnergy extends App {

  require(args.length == 1, "Usage:\njava ShowEnergy <image filename>")

  val inputPicture = Picture(args(0))
  val seamCarver = SeamCarver(inputPicture)
  val outputPicture = Utils.energyPicture(seamCarver)

  inputPicture display (title = "Input")
  outputPicture display (title = "Output")
}

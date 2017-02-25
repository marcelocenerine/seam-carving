package com.cenerino.seamcarving

object ResizeDemo extends App {

  require(args.length == 3, "Usage:\njava ResizeDemo <image filename> <num cols to remove> <num rows to remove>")

  val start = System.currentTimeMillis()
  val filename = args(0)
  val removeCols = args(1).toInt
  val removeRows = args(2).toInt
  val inputPicture = Picture(filename)
  val seamCarver = SeamCarver(inputPicture)

  println(s"Input image is ${inputPicture.width} columns by ${inputPicture.height} rows")

  for (row <- 1 to removeRows) {
    val seam = seamCarver.findHorizontalSeam
    //println(s"Removing horizontal seam #$row of $removeRows")
    seamCarver removeHorizontalSeam seam
  }

  for (col <- 1 to removeCols) {
    val seam = seamCarver.findVerticalSeam
    //println(s"Removing vertical seam #$col of $removeCols")
    seamCarver removeVerticalSeam seam
  }

  val outputPicture = seamCarver.picture

  println(s"Output image is ${outputPicture.width} columns by ${outputPicture.height} rows")
  println(s"Total time: ${System.currentTimeMillis() - start}")

  inputPicture display (title = s"Input ${inputPicture.width} x ${inputPicture.height}")
  outputPicture display (title = s"Output ${outputPicture.width} x ${outputPicture.height}")
}

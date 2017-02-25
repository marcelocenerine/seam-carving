package com.cenerino.seamcarving.demo

import com.cenerino.seamcarving.{Image, SeamCarver}

object ResizeDemo extends App {

  require(args.length == 3, "Usage:\njava ResizeDemo <image filename> <num cols to remove> <num rows to remove>")

  val start = System.currentTimeMillis()
  val filename = args(0)
  val removeCols = args(1).toInt
  val removeRows = args(2).toInt
  val input = Image(filename)
  var output = input

  println(s"Input image is ${input.width} columns by ${input.height} rows")

  for (row <- 1 to removeRows) {
    val seam = SeamCarver.nextHorizontalSeam(output)
    //println(s"Removing horizontal seam #$row of $removeRows")
    output = SeamCarver.removeHorizontalSeam(seam, output)
  }

  for (col <- 1 to removeCols) {
    val seam = SeamCarver.nextVerticalSeam(output)
    //println(s"Removing vertical seam #$col of $removeCols")
    output = SeamCarver.removeVerticalSeam(seam, output)
  }

  println(s"Output image is ${output.width} columns by ${output.height} rows")
  println(s"Total time: ${System.currentTimeMillis() - start}")

  new Frame(title = s"Input ${input.width} x ${input.height}", image = input).show()
  new Frame(title = s"Output ${output.width} x ${output.height}", image = output).show()
}

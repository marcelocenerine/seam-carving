package com.cenerino.seamcarving

import java.awt.image.BufferedImage
import java.awt.image.BufferedImage.TYPE_INT_RGB
import java.io.File
import javax.imageio.ImageIO

class Image(private[this] val pixels: Array[Array[Int]]) {

  def width: Int = pixels.length

  def height: Int = if (width > 0) pixels.head.length else 0

  def rgb(pos: Pos): Int = {
    val (col, row) = pos

    require(col >= 0 && col < width)
    require(row >= 0 && row < height)

    pixels(col)(row)
  }

  def transpose: Image = new Image(pixels transpose)

  override def equals(that: Any): Boolean = that match {
    case image: Image =>
      if (image.width != this.width || image.height != this.height) false
      else {
        val allPos = for (c <- 0 until width; r <- 0 until height) yield (c, r)
        allPos.forall(pos => image.rgb(pos) == this.rgb(pos))
      }
    case _ => false
  }
}

object Image {

  def apply(path: String): Image = {
    val file = new File(path)
    val bimg = ImageIO.read(if (file.isFile) file.toURI.toURL else getClass.getResource(path))
    val pixels = Array.tabulate(bimg.getWidth, bimg.getHeight)(bimg.getRGB(_, _))
    new Image(pixels)
  }

  def apply(pixels: Array[Array[Int]]): Image = new Image(pixels) // TODO safe copy

  implicit def image2BufferedImage(image: Image): BufferedImage = {
    val bufferedImage = new BufferedImage(image.width, image.height, TYPE_INT_RGB)

    for {
      col <- 0 until image.width
      row <- 0 until image.height
    } bufferedImage.setRGB(col, row, image.rgb(col, row))

    bufferedImage
  }
}

package com.cenerino.seamcarving

import java.awt.image.BufferedImage
import java.awt.image.BufferedImage.TYPE_INT_RGB
import java.io.{File, FileNotFoundException}
import javax.imageio.ImageIO

class Image private(private val pixels: Array[Array[RGB]], val width: Int, val height: Int) extends PartialFunction[Pos, RGB]{

  def apply(pos: Pos): RGB = {
    require(isDefinedAt(pos), "Invalid coordinates")
    pixels(pos._1)(pos._2)
  }

  override def isDefinedAt(pos: Pos): Boolean = {
    val (col, row) = pos
    col >= 0 && col < width && row >= 0 && row < height
  }

  // TODO remove?
  def update(pos: Pos, rgb: RGB): Unit = {
    require(isDefinedAt(pos), "Invalid coordinates")
    pixels(pos._1)(pos._2) = rgb
  }

  def transpose: Image = {
    new Image(pixels.transpose, height, width)
  }

  // FIXME
  override def equals(that: Any): Boolean = that match {
    case other: Image => this.pixels sameElements other.pixels
    case _ => false
  }
}

object Image {

  def apply(path: String): Image = {
    load(path) match {
      case Some(img) => {
        val (width, height) = (img.getWidth, img.getHeight)
        val pixels = Array.tabulate[RGB](width, height)(img.getRGB(_, _))
        new Image(pixels, width, height)
      }
      case None => throw new FileNotFoundException
    }
  }

  private def load(path: String): Option[BufferedImage] = {
    val file = new File(path)
    Option {
      if (file.isFile) file.toURI.toURL
      else getClass.getResource(path)
    }.map(ImageIO.read)
  }

  def blank(width: Int, height: Int, color: RGB = White): Image = new Image(Array.ofDim(width, height), width, height)

  implicit def image2BufferedImage(image: Image): BufferedImage = {
    val bufferedImage = new BufferedImage(image.width, image.height, TYPE_INT_RGB)

    for {
      row <- 0 until image.height
      col <- 0 until image.width
    } bufferedImage.setRGB(col, row, image(col, row))

    bufferedImage
  }
}

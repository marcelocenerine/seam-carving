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

  def removeVerticalSeam(seam: Seam): Image = {
    require(width > 1, "Image cannot be vertically resized")
    require(seam.length == height, "Seam length does not match image height")
    require(seam forall(isDefinedAt), "Seam contains invalid coordinates")

    val newWidth = width - 1
    val output = Array.ofDim[RGB](newWidth, height)

    for {
      (col, row) <- seam
      targetCol <- 0 until newWidth
      sourceCol = if (targetCol < col) targetCol else targetCol + 1
    } output(targetCol)(row) = pixels(sourceCol)(row)

    new Image(output, newWidth, height)
  }

  def removeHorizontalSeam(seam: Seam): Image = {
    require(height > 1, "Image cannot be horizontally resized")
    require(seam.length == width, "Seam length does not match image width")
    require(seam forall(isDefinedAt), "Seam contains invalid coordinates")

    val newHeight = height - 1
    val output = Array.ofDim[RGB](width, newHeight)

    for {
      (col, row) <- seam
      targetRow <- 0 until newHeight
      sourceRow = if (targetRow < row) targetRow else targetRow + 1
    } output(col)(targetRow) = pixels(col)(sourceRow)

    new Image(output, width, newHeight)
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

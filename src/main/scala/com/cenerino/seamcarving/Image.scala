package com.cenerino.seamcarving

import Image._
import java.awt.image.BufferedImage
import java.awt.image.BufferedImage.TYPE_INT_RGB
import java.io.{File, FileNotFoundException}
import javax.imageio.ImageIO

class Image private(private val pixels: Array[RGB], val width: Int, val height: Int) extends PartialFunction[Pos, RGB]{

  def apply(pos: Pos): RGB = {
    require(isDefinedAt(pos), "Invalid coordinates")
    pixels(to1DimIndex(pos, width))
  }

  override def isDefinedAt(pos: Pos): Boolean = {
    val (col, row) = pos
    col >= 0 && col < width && row >= 0 && row < height
  }

  def update(pos: Pos, rgb: RGB): Unit = {
    require(isDefinedAt(pos), "Invalid coordinates")
    pixels(to1DimIndex(pos, width)) = rgb
  }

  def transpose: Image = {
    val transposedPixels = Array.ofDim[RGB](width * height)

    for {
      row <- 0 until height
      col <- 0 until width
      sourcePx = to1DimIndex((col, row), width)
      targetPx = to1DimIndex((row, col), height)
    } transposedPixels(targetPx) = pixels(sourcePx)

    new Image(transposedPixels, height, width)
  }

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
        val pixels = Array.tabulate[RGB](width * height)(i => {
          val (c, r) = to2DimIndex(i, width)
          img.getRGB(c, r)
        })
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

  def blank(width: Int, height: Int, color: RGB = White): Image = new Image(Array.fill(color)(width * height), width, height)

  private def to2DimIndex(index: Int, width: Int): Pos = (index % width, index / width)
  private def to1DimIndex(pos: Pos, width: Int): Int = pos._1 + (pos._2 * width)

  implicit def image2BufferedImage(image: Image): BufferedImage = {
    val bufferedImage = new BufferedImage(image.width, image.height, TYPE_INT_RGB)

    for {
      row <- 0 until image.height
      col <- 0 until image.width
    } bufferedImage.setRGB(col, row, image(col, row))

    bufferedImage
  }
}

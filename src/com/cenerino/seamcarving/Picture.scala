package com.cenerino.seamcarving

import java.awt.image.BufferedImage
import java.io.File
import java.net.URL
import javax.imageio.ImageIO

class Picture private(private val image: BufferedImage) {

  def this(path: String) {
    this {
      val file = new File(path)

      if (file.isFile) ImageIO.read(file)
      else
        Option(getClass.getResource(path))
          .orElse(Option(new URL(path)))
          .map(url => ImageIO.read(url))
          .get
    }
  }

  def this(width: Int, height: Int) {
    this(new BufferedImage(width, height, 1))
  }

  def width: Int = image.getWidth

  def height: Int = image.getHeight

  def setRGB(x: Int, y: Int, rgb: Int): Unit = {
    assertIndexesAreWithinBounds(x, y)
    image.setRGB(x, y, rgb)
  }

  def getRGB(x: Int, y: Int): Int = {
    assertIndexesAreWithinBounds(x, y)
    image.getRGB(x, y)
  }

  private def assertIndexesAreWithinBounds(x: Int, y: Int): Unit = {
    if (x < 0 || x >= width) throw new IndexOutOfBoundsException("Invalid x index")
    if (y < 0 || y >= height) throw new IndexOutOfBoundsException("Invalid y index")
  }

  def show: Unit = {
    // TODO
  }
}

object Picture {
  def apply(path: String): Picture = new Picture(path)
  def apply(width: Int, height: Int): Picture = new Picture(width, height)
}

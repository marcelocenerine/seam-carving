package com.cenerino.seamcarving

import java.awt.FlowLayout
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import javax.swing.{JFrame, ImageIcon, JLabel}

class Image(private val pixels: Array[Array[Int]]) {

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

  // TODO move somewhere else
  def display(title: String = ""): Unit = {
    val image = new BufferedImage(width, height, 1)
    for { c <- 0 until width; r <- 0 until height } image.setRGB(c, r, pixels(c)(r))


    val label = new JLabel()
    label.setIcon(new ImageIcon(image))
    val frame = new JFrame()
    frame.setTitle(title)
    frame.add(label)
    frame.setLayout(new FlowLayout())
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.pack()
    frame.setVisible(true)
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
}

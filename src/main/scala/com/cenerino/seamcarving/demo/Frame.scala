package com.cenerino.seamcarving.demo

import java.awt.FlowLayout
import java.awt.image.BufferedImage
import javax.swing.{ImageIcon, JFrame, JLabel}

import com.cenerino.seamcarving.Image
import com.cenerino.seamcarving.Image.image2BufferedImage

class Frame(image: Image, title: String = "") {

  val bufferedImage: BufferedImage = image

  def show(): Unit = {
    val frame = new JFrame()
    frame.setTitle(title)
    frame.add(new JLabel(new ImageIcon(bufferedImage)))
    frame.setLayout(new FlowLayout())
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.pack()
    frame.setVisible(true)
  }
}


//val drawSeam = (seam: Seam) => seam.foreach { case (c, r) => pixels(c)(r) = RED.getRGB }
//
//if (showVerticalSeam) drawSeam(nextVerticalSeam(image))
//if (showHorizontalSeam) drawSeam(nextHorizontalSeam(image))
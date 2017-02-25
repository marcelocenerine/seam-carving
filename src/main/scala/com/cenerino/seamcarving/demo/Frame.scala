package com.cenerino.seamcarving.demo

import java.awt.FlowLayout
import java.awt.image.BufferedImage
import javax.swing.{ImageIcon, JFrame, JLabel}

import com.cenerino.seamcarving.Image

class Frame(val title: String, val image: Image) {

  def show(): Unit = {
    val bufferedImage = new BufferedImage(image.width, image.height, BufferedImage.TYPE_INT_RGB)
    for (c <- 0 until image.width; r <- 0 until image.height) bufferedImage.setRGB(c, r, image.rgb(c, r))

    val frame = new JFrame()
    frame.setTitle(title)
    frame.add(new JLabel(new ImageIcon(bufferedImage)))
    frame.setLayout(new FlowLayout())
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.pack()
    frame.setVisible(true)
  }
}

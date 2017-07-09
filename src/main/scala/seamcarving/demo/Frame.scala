package seamcarving.demo

import java.awt.FlowLayout
import java.awt.image.BufferedImage
import javax.swing.{ImageIcon, JFrame, JLabel}

import seamcarving.Image.image2BufferedImage
import seamcarving.Image

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
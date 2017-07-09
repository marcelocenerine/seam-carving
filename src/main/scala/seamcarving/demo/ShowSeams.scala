package seamcarving.demo

import seamcarving.SeamCarver._
import seamcarving._

object ShowSeams extends App {

  require(args.length == 3, "Usage:\njava ShowSeams <image filename> <show vertical seam> <show horizontal seam>")

  val input = Image(args(0))
  val showVerticalSeam = args(1).toBoolean
  val showHorizontalSeam = args(2).toBoolean
  var output = EnergyFunction.dualGradient(input).energyPicture

  if (showHorizontalSeam) {
    val seam = nextHorizontalSeam(input)
    output = output.drawn(seam, Red)
  }

  if (showVerticalSeam) {
    val seam = nextVerticalSeam(input)
    output = output.drawn(seam, Red)
  }

  new Frame(title = "Input", image = input).show()
  new Frame(title = "Output", image = output).show()
}

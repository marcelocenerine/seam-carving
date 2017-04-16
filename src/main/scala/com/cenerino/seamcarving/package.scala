package com.cenerino

package object seamcarving {

  type Pos = (Int, Int)
  type Seam = Seq[Pos]
  type RGB = Int

  def red(rgb: RGB): Int = (rgb >> 16) & 0xFF
  def green(rgb: RGB): Int = (rgb >> 8) & 0xFF
  def blue(rgb: RGB): Int = (rgb >> 0) & 0xFF

  def rgb(r: Int, g: Int, b: Int): RGB = (r << 16) | (g << 8) | (b << 0)

  def rgb(r: Float, g: Float, b: Float): RGB = {
    def convert(v: Float) = (v * 255 + 0.5).toInt
    rgb(convert(r), convert(g), convert(b))
  }

  val Black: RGB = rgb(0, 0, 0)
  val Red: RGB = rgb(255, 0, 0)
  val White: RGB = rgb(255, 255, 255)
}

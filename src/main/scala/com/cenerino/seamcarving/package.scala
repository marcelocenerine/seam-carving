package com.cenerino

package object seamcarving {

  type Pos = (Int, Int)
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
  val Blue: RGB = rgb(0, 0, 255)
  val Green: RGB = rgb(0, 128, 0)
  val Grey: RGB = rgb(128, 128, 128)
  val Orange: RGB = rgb(255, 165, 0)
  val Purple: RGB = rgb(128, 0, 128)
  val Red: RGB = rgb(255, 0, 0)
  val White: RGB = rgb(255, 255, 255)
  val Yellow: RGB = rgb(255, 255, 0)
}

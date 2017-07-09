package seamcarving

import java.awt.image.BufferedImage
import java.io.FileNotFoundException
import javax.imageio.ImageIO

import org.scalatest.FunSuite

class ImageSuite extends FunSuite {

  val path3x4 = "/3x4.png"
  val path10x10 = "/10x10.png"

  val pixels1x1 = Array(Array(Blue))

  val pixels2x2 = Array(
    Array(Red, Black),
    Array(Blue, White)
  )

  val pixels3x3 = Array(
    Array(Red, Black, White),
    Array(Blue, Green, Yellow),
    Array(Purple, Orange, Grey)
  )

  test("should create image from file in the classpath") {
    val image = Image(path10x10)

    assert(image.width === 10)
    assert(image.height === 10)
  }

  test("should create image from file system") {
    val path = getClass.getResource(path3x4).getPath
    val image = Image(path)

    assert(image.width === 3)
    assert(image.height === 4)
  }

  test("should fail to create image from invalid path") {
    assertThrows[FileNotFoundException]{
      Image("foo")
    }
  }

  test("should create image from array of pixels") {
    val image = Image(pixels3x3)

    assert(image.width === 3)
    assert(image.height === 3)
  }

  test("should fail to create image from empty array of pixels") {
    assertThrows[IllegalArgumentException] {
      Image(Array.empty[Array[RGB]])
    }
  }

  test("should fail to create image from array of pixels with inconsistent row sizes") {
    assertThrows[IllegalArgumentException] {
      val pixels = Array(Array(Red), Array(Black, White))
      Image(pixels)
    }
  }

  test("should convert image to buffered image") {
    val image = Image(path3x4)
    val converted: BufferedImage = image // implicit conversion

    val expected = ImageIO.read(getClass.getResource(path3x4))

    assert(converted.getWidth === expected.getWidth)
    assert(converted.getHeight === expected.getHeight())

    for (c <- 0 until expected.getWidth; r <- 0 until expected.getHeight) {
      assert(converted.getRGB(c, r) === expected.getRGB(c, r))
    }
  }

  test("should return color of specific pixels") {
    val image = Image(pixels2x2)

    assert(image(0, 0) === Red)
    assert(image(0, 1) === Black)
    assert(image(1, 0) === Blue)
    assert(image(1, 1) === White)
  }

  test("should fail to get color if column coordinate is invalid") {
    val image = Image(pixels2x2)

    assertThrows[IllegalArgumentException] {
      image(2, 0)
    }
  }

  test("should fail to get color if row coordinate is invalid") {
    val image = Image(pixels2x2)

    assertThrows[IllegalArgumentException] {
      image(0, -2)
    }
  }

  test("should be defined at valid coordinates") {
    assert(Image(pixels2x2).isDefinedAt(0, 0))
    assert(Image(pixels2x2).isDefinedAt(0, 1))
    assert(Image(pixels2x2).isDefinedAt(1, 0))
    assert(Image(pixels2x2).isDefinedAt(1, 1))
  }

  test("should not be defined at column coordinate >= width") {
    assert(!Image(pixels2x2).isDefinedAt(2, 0))
  }

  test("should not be defined at negative column coordinate") {
    assert(!Image(pixels2x2).isDefinedAt(-2, 0))
  }

  test("should not be defined at row coordinate >= height") {
    assert(!Image(pixels2x2).isDefinedAt(0, 2))
  }

  test("should not be defined at negative row coordinate") {
    assert(!Image(pixels2x2).isDefinedAt(0, -2))
  }

  test("should clone") {
    val original = Image(path3x4)
    val copy = original.clone

    assert(copy === original)
    assert(copy ne original)
  }

  test("should be equals") {
    val image1 = Image(path10x10)
    val image2 = Image(path10x10)

    assert(image1 === image2)
  }

  test("should not be equals") {
    val image1 = Image(path3x4)
    val image2 = Image(path10x10)

    assert(image1 !== image2)
  }

  test("should remove horizontal seam") {
    val input = Image(pixels3x3)
    val seam = HorizontalSeam(Seq((0, 0), (1, 1), (2, 1)))
    val output = input removed seam

    assert(output.width === 3)
    assert(output.height === 2)
    assert(output(0, 0) === Black)
    assert(output(0, 1) === White)
    assert(output(1, 0) === Blue)
    assert(output(1, 1) === Yellow)
    assert(output(2, 0) === Purple)
    assert(output(2, 1) === Grey)
  }

  test("should horizontally remove traversal seam") {
    val input = Image(pixels3x3)
    val seam = HorizontalSeam(Seq((0, 0), (1, 1), (2, 2)))
    val output = input removed seam

    assert(output.width === 3)
    assert(output.height === 2)
    assert(output(0, 0) === Black)
    assert(output(0, 1) === White)
    assert(output(1, 0) === Blue)
    assert(output(1, 1) === Yellow)
    assert(output(2, 0) === Purple)
    assert(output(2, 1) === Orange)
  }

  test("should fail to remove horizontal seam if image width == 1") {
    val input = Image(pixels1x1)
    val seam = HorizontalSeam(Seq((0, 0)))

    assertThrows[IllegalArgumentException] {
      input removed seam
    }
  }

  test("should fail to remove horizontal seam if length < image width") {
    val input = Image(pixels2x2)
    val seam = HorizontalSeam(Seq((0, 0)))

    assertThrows[IllegalArgumentException] {
      input removed seam
    }
  }

  test("should fail to remove horizontal seam if length > image width") {
    val input = Image(pixels2x2)
    val seam = HorizontalSeam(Seq((0, 0), (1, 0), (2, 0)))

    assertThrows[IllegalArgumentException] {
      input removed seam
    }
  }

  test("should fail to remove horizontal seam if it contains invalid coordinates") {
    val input = Image(pixels2x2)
    val seam = HorizontalSeam(Seq((0, 1), (1, 2)))

    assertThrows[IllegalArgumentException] {
      input removed seam
    }
  }

  test("should remove vertical seam") {
    val input = Image(pixels3x3)
    val seam = VerticalSeam(Seq((1, 0), (0, 1), (1, 2)))
    val output = input removed seam

    assert(output.width === 2)
    assert(output.height === 3)
    assert(output(0, 0) === Red)
    assert(output(0, 1) === Green)
    assert(output(0, 2) === White)
    assert(output(1, 0) === Purple)
    assert(output(1, 1) === Orange)
    assert(output(1, 2) === Grey)
  }

  test("should vertically remove traversal seam") {
    val input = Image(pixels3x3)
    val seam = VerticalSeam(Seq((0, 0), (1, 1), (2, 2)))
    val output = input removed seam

    assert(output.width === 2)
    assert(output.height === 3)
    assert(output(0, 0) === Blue)
    assert(output(0, 1) === Black)
    assert(output(0, 2) === White)
    assert(output(1, 0) === Purple)
    assert(output(1, 1) === Orange)
    assert(output(1, 2) === Yellow)
  }

  test("should fail to remove vertical seam if image height == 1") {
    val input = Image(pixels1x1)
    val seam = VerticalSeam(Seq((0, 0)))

    assertThrows[IllegalArgumentException] {
      input removed seam
    }
  }

  test("should fail to remove vertical seam if length < image height") {
    val input = Image(pixels2x2)
    val seam = VerticalSeam(Seq((0, 0)))

    assertThrows[IllegalArgumentException] {
      input removed seam
    }
  }

  test("should fail to remove vertical seam if length > image height") {
    val input = Image(pixels2x2)
    val seam = VerticalSeam(Seq((0, 0), (0, 1), (0, 2)))

    assertThrows[IllegalArgumentException] {
      input removed seam
    }
  }

  test("should fail to remove vertical seam if it contains invalid coordinates") {
    val input = Image(pixels2x2)
    val seam = VerticalSeam(Seq((1, 0), (2, 1)))

    assertThrows[IllegalArgumentException] {
      input removed seam
    }
  }
}

package seamcarving

import org.scalatest.FunSuite

class SeamSuite extends FunSuite {

  test("should create horizontal seam with pixels in the same row") {
    val path = Seq((0, 0), (1, 0), (2, 0), (3, 0), (4, 0))
    val seam = HorizontalSeam(path)

    assert(seam.length === path.length)
    (seam zip path) foreach { case (pos1, pos2) => assert(pos1 === pos2) }
  }

  test("should create horizontal seam with pixels in adjacent rows") {
    val path = Seq((0, 0), (1, 1), (2, 2), (3, 1), (4, 0))
    val seam = HorizontalSeam(path)

    assert(seam.length === path.length)
    (seam zip path) foreach { case (pos1, pos2) => assert(pos1 === pos2) }
  }

  test("should create horizontal seam with single pixel") {
    val seam = HorizontalSeam(Seq((0, 0)))

    assert(seam.length === 1)
  }

  test("should fail to create empty horizontal seam") {
    assertThrows[IllegalArgumentException] {
      HorizontalSeam(Seq.empty)
    }
  }

  test("should fail to create horizontal seam if next pixel is at the same column") {
    val path = Seq((0, 0), (0, 1), (0, 2))

    assertThrows[IllegalArgumentException] {
      HorizontalSeam(path)
    }
  }

  test("should fail to create horizontal seam if next pixel is before current column") {
    val path = Seq((0, 0), (1, 1), (0, 2))

    assertThrows[IllegalArgumentException] {
      HorizontalSeam(path)
    }
  }

  test("should fail to create horizontal seam if next pixel is > 1 position ahead of current column") {
    val path = Seq((0, 0), (2, 1), (3, 2))

    assertThrows[IllegalArgumentException] {
      HorizontalSeam(path)
    }
  }

  test("should fail to create horizontal seam if next pixel is > 1 position above the current row") {
    val path = Seq((0, 3), (1, 0), (2, 0))

    assertThrows[IllegalArgumentException] {
      HorizontalSeam(path)
    }
  }

  test("should fail to create horizontal seam if next pixel is > 1 position below the current row") {
    val path = Seq((0, 0), (1, 2), (2, 2))

    assertThrows[IllegalArgumentException] {
      HorizontalSeam(path)
    }
  }

  test("should create vertical seam with pixels in the same column") {
    val path = Seq((0, 0), (0, 1), (0, 2), (0, 3), (0, 4))
    val seam = VerticalSeam(path)

    assert(seam.length === path.length)
    (seam zip path) foreach { case (pos1, pos2) => assert(pos1 === pos2) }
  }

  test("should create vertical seam with pixels in adjacent columns") {
    val path = Seq((0, 0), (1, 1), (2, 2), (1, 3), (0, 4))
    val seam = VerticalSeam(path)

    assert(seam.length === path.length)
    (seam zip path) foreach { case (pos1, pos2) => assert(pos1 === pos2) }
  }

  test("should create vertical seam with single pixel") {
    val seam = VerticalSeam(Seq((0, 0)))

    assert(seam.length === 1)
  }

  test("should fail to create empty vertical seam") {
    assertThrows[IllegalArgumentException] {
      VerticalSeam(Seq.empty)
    }
  }

  test("should fail to create vertical seam if next pixel is at the same row") {
    val path = Seq((0, 0), (1, 0), (2, 0))

    assertThrows[IllegalArgumentException] {
      VerticalSeam(path)
    }
  }

  test("should fail to create vertical seam if next pixel is before current row") {
    val path = Seq((0, 0), (1, 1), (1, 0))

    assertThrows[IllegalArgumentException] {
      VerticalSeam(path)
    }
  }

  test("should fail to create vertical seam if next pixel is +1 position ahead of current row") {
    val path = Seq((0, 0), (0, 2), (0, 3))

    assertThrows[IllegalArgumentException] {
      VerticalSeam(path)
    }
  }

  test("should fail to create vertical seam if next pixel is +1 position to the left of current column") {
    val path = Seq((3, 0), (0, 1), (0, 2))

    assertThrows[IllegalArgumentException] {
      VerticalSeam(path)
    }
  }

  test("should fail to create vertical seam if next pixel is > 1 position to the right of current column") {
    val path = Seq((0, 0), (2, 1), (2, 2))

    assertThrows[IllegalArgumentException] {
      VerticalSeam(path)
    }
  }
}

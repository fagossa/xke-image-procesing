package fr.xebia.image

import fr.xebia.image.ImagingTools._
import org.scalatest.{FunSpec, Matchers}

class ScalaTestExampleSpec extends FunSpec with Matchers {

  describe("a segmentation monad") {

    it("should replace '#' by '@'") {
      // given an image
      val rawImage = TestImageBuilder.fromString(
        """
          |......###........
          |...###...##......
          |..##.......##....
          |..#..........#...
          |..#..........#...
          |..#.........##...
          |..###......###...
          |...###...###.....
          |.....####........
          |.................
        """.stripMargin)

      // when
      val segmentedImg = ImageProcessingMonad[String](rawImage).threshold(
        cell => cell == "#",
        replaceBy = "@"
      )

      // then
      segmentedImg.rawImage shouldBe TestImageBuilder.fromString(
        """
          |......@@@........
          |...@@@...@@......
          |..@@.......@@....
          |..@..........@...
          |..@..........@...
          |..@.........@@...
          |..@@@......@@@...
          |...@@@...@@@.....
          |.....@@@@........
          |.................
        """.stripMargin
      )
    }

    it("should propagate a front from a specified seed") {
      val specialChar = "@"
      // given
      val rawImage = TestImageBuilder.fromString(
        """
          |......###........
          |...###...##......
          |..##.......##....
          |..############...
          |..#..........#...
          |..#.........##...
          |..############...
          |...###...###.....
          |.....####........
          |.................
        """.stripMargin)
      val seed = Position(0, 7)

      // when
      val monad = ImageProcessingMonad[String](rawImage)

      val segmentedPositions = monad.propagateFront(
        seeds = monad.rawImage.neighborsAndSelf(seed),
        searchedValue = "#",
        markWith = specialChar
      )
      val segmentedImage = monad.replace(segmentedPositions, specialChar)

      // then
      segmentedImage.rawImage shouldBe TestImageBuilder.fromString(
        """
          |......@@@........
          |...@@@...@@......
          |..@@.......@@....
          |..@@@@@@@@@@@@...
          |..@..........@...
          |..@.........@@...
          |..@@@@@@@@@@@@...
          |...@@@...@@@.....
          |.....@@@@........
          |.................
        """.stripMargin
      )
    }

  }

  describe("a front propagation monad") {

    val rawImage = TestImageBuilder.fromString(
      """|......###........
        |...###...##......
        |..##.......##....
        |..############...
        |.................
        |.................
        |..############...
        |...#########.....
        |.....####........
        |.................
      """.stripMargin)

    it("should detect a missing first match in the image") {
      ImageProcessingMonad[String](rawImage)
        .getFirstThatMatches("&") shouldNot be(defined)
    }

    it("should propagate a front from the first value that matches") {
      // given
      val firstFrontMonad = ImageProcessingMonad[String](rawImage)
      val firstSeed = aSeedThatMatches(firstFrontMonad, Position(0, 6), "#")

      // when the first monad is called
      val firstFront: List[Position] = firstFrontMonad.propagateFront(
        seeds = List(firstSeed),
        searchedValue = "#",
        markWith = "@"
      )
      firstFront shouldNot be(empty)
      firstFrontMonad.replace(firstFront, "@").rawImage.writeToFile("firstSegmentation.txt")

      // when the second monad
      val secondFrontMonad = firstFrontMonad.replace(firstFront, "@")
      val secondSeed = aSeedThatMatches(secondFrontMonad, Position(6, 2), "#")

      val secondFront: List[Position] = secondFrontMonad.propagateFront(
        seeds = List(secondSeed),
        searchedValue = "#",
        markWith = "&"
      )
      //secondFrontMonad.replace(secondFront, "&").rawImage.writeToFile("secondSegmentation.txt")
      secondFrontMonad.replace(secondFront, "&").rawImage shouldBe TestImageBuilder.fromString(
        """
          |......@@@........
          |...@@@...@@......
          |..@@.......@@....
          |..@@@@@@@@@@@@...
          |.................
          |.................
          |..&&&&&&&&&&&&...
          |...&&&&&&&&&.....
          |.....&&&&........
          |.................
        """.stripMargin
      )
    }

  }

}

object ImagingTools {

  def aSeedThatMatches(processingMonad: ImageProcessingMonad[String], position: Position, expectedValue: String): Position = {
    val firstSeed = processingMonad
      .getFirstThatMatches("#")
      .getOrElse(throw new IllegalStateException("# not found"))
    assert(firstSeed == position)
    assert(processingMonad.rawImage.at(firstSeed) == "#")
    firstSeed
  }

  def writeToFile(processingMonad: ImageProcessingMonad[String], front: List[Position], fileName: String, newContent: String = "@") =
    processingMonad.replace(front, newContent).rawImage.writeToFile(fileName)

  object TestImageBuilder {

    def fromString(rawContent: String): RawImage[String] = {
      val contents: List[List[String]] = rawContent
        .split("\n")
        .map(_.toCharArray.toList.map(_.toString))
        .toList
        .filter(_.map(_.trim).mkString.nonEmpty)
      RawImage(contents)
    }

  }

  def anImageMonad(content: String): ImageProcessingMonad[String] =
    ImageProcessingMonad(TestImageBuilder.fromString(content))

}

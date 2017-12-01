package fr.xebia.image.core

import fr.xebia.image.TestFactory._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{Matchers, WordSpec}

class RawImageSpec extends WordSpec with Matchers with ScalaFutures {

  "an image wrapper" which {

    val aDummyContent = List(
      List("a", "b"),
      List("c", "d"),
      List("c", "e")
    )

    "validates input" must {

      "detect an empty input" in {
        intercept[IllegalArgumentException] {
          aStringImage.filledWith(List.empty).get
        }
      }

      "detect images having rows with different sizes" in {
        val anErroneousContent = List(
          List("a", "b"),
          List("a")
        )
        intercept[IllegalArgumentException] {
          aStringImage.filledWith(anErroneousContent).get
        }
      }

    }

    "gets data on content" must {
      val aRealisticContent =
        """
          |.................
          |...##......##....
          |...##......##....
          |...##......##....
          |.................
          |.................
          |..##........##...
          |...##......##....
          |.....######......
          |.................
        """.stripMargin

      "return the right content at the position specified" in {
        aStringImage.filledWith(
          aDummyContent
        ).foreach { anImage =>
          anImage at Position(0, 0) shouldBe "a"
          anImage at Position(0, 1) shouldBe "c"
          anImage at Position(0, 2) shouldBe "c"

          anImage at Position(1, 0) shouldBe "b"
          anImage at Position(1, 1) shouldBe "d"
          anImage at Position(1, 2) shouldBe "e"
        }
      }

      "filter the neighborhood of a pixel close to the border" in {
        val anImage = anImageFunctorFrom(aRealisticContent).image

        anImage.neighborsOnly(Position(0, 0)).sorted shouldBe Seq(
          Position(1, 0),
          Position(1, 1),
          Position(0, 1)
        ).sorted

        anImage.neighborsAndSelf(Position(0, 0)).sorted shouldBe Seq(
          Position(0, 0),
          Position(1, 0),
          Position(1, 1),
          Position(0, 1)
        ).sorted
      }

      "get the neighborhood of a pixel not in the border" in {
        val x = 8
        val y = 5
        val anImage = anImageFunctorFrom(aRealisticContent).image

        anImage.neighborsOnly(Position(x, y)).sorted shouldBe Seq(
          // first line
          Position(x - 1, y - 1),
          Position(x, y - 1),
          Position(x + 1, y - 1),
          // second line
          Position(x - 1, y),
          Position(x + 1, y),
          // third line
          Position(x - 1, y + 1),
          Position(x, y + 1),
          Position(x + 1, y + 1)
        ).sorted

        anImage.neighborsAndSelf(Position(x, y)).sorted shouldBe Seq(
          // first line
          Position(x - 1, y - 1),
          Position(x, y - 1),
          Position(x + 1, y - 1),
          // second line
          Position(x - 1, y),
          Position(x, y),
          Position(x + 1, y),
          // third line
          Position(x - 1, y + 1),
          Position(x, y + 1),
          Position(x + 1, y + 1)
        ).sorted
      }

      "get the first that matches" in {
        aStringImage.filledWith(
          aDummyContent
        ).foreach { anImage =>
          anImage firstThatMatches "a" shouldBe Some(Position(0, 0))
          anImage firstThatMatches "c" shouldBe Some(Position(0, 1))
          anImage firstThatMatches "e" shouldBe Some(Position(1, 2))
        }
      }

      "return None when no match found" in {
        aStringImage.filledWith(
          aDummyContent
        ).foreach { anImage =>
          anImage firstThatMatches "x" shouldBe None
          anImage firstThatMatches "y" shouldBe None
        }
      }

    }

    "process data" must {

      "replace the position specified" in {
        val positionsToReplace = List(Position(0, 0), Position(1, 0))
        val expectedImage = RawImage.buildFrom[String](
          List(
            List("@", "@"),
            List("c", "d"),
            List("c", "e")
          )
        )

        aStringImage.filledWith(aDummyContent).map(_.replace(positionsToReplace, "@")) shouldBe expectedImage
      }

    }

  }

}

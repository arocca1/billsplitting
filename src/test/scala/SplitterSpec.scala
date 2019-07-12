import org.scalatest._
import org.scalatest.Matchers._

class SplitterSpec extends WordSpec {
  "split" should {
    "error when negative tip percentage" in {
      assertThrows[Exception] {
        Splitter.split(Map("corn" -> Splitter.ItemPriceAndCount(1.0, 1)), Map("antonio" -> Seq(Splitter.PersonItemCounts("corn", 1))), -0.1, 0.12)
      }
    }
    "error when negative tax rate" in {
      assertThrows[Exception] {
        Splitter.split(Map("corn" -> Splitter.ItemPriceAndCount(1.0, 1)), Map("antonio" -> Seq(Splitter.PersonItemCounts("corn", 1))), 0.9, -0.12)
      }
    }
    "return empty for empty items and empty person selects" in {
      Splitter.split(Map.empty[String, Splitter.ItemPriceAndCount], Map.empty[String, Seq[Splitter.PersonItemCounts]], 0.18, 0.13) shouldBe empty
    }
    "error when items is empty, but person selections are not" in {
      assertThrows[Exception] {
        Splitter.split(Map.empty[String, Splitter.ItemPriceAndCount], Map("antonio" -> Seq(Splitter.PersonItemCounts("corn", 1))), 0.18, 0.13)
      }
    }
    "error when person selections are empty, but items are not" in {
      assertThrows[Exception] {
        Splitter.split(Map("corn" -> Splitter.ItemPriceAndCount(1.0, 1)), Map.empty[String, Seq[Splitter.PersonItemCounts]], 0.18, 0.13)
      }
    }
    "split correctly among 2 people" in {
      Splitter.split(
        Map(
          "corn" -> Splitter.ItemPriceAndCount(1.5, 2),
          "steak" -> Splitter.ItemPriceAndCount(20, 1),
          "beans" -> Splitter.ItemPriceAndCount(2, 3),
          "burrito" -> Splitter.ItemPriceAndCount(13, 1)
        ),
        Map(
          "antonio" -> Seq(
            Splitter.PersonItemCounts("corn", 1),
            Splitter.PersonItemCounts("steak", 1),
            Splitter.PersonItemCounts("beans", 2)
          ),
          "vlad" -> Seq(
            Splitter.PersonItemCounts("corn", 1),
            Splitter.PersonItemCounts("burrito", 1),
            Splitter.PersonItemCounts("beans", 1)
          )
        ),
        0.1,
        0.1
      ) should equal(Map(
        "antonio" -> Splitter.PersonTotalOwing(Map(
          "corn" -> Splitter.ItemPriceAndCount(1.5, 1),
          "steak" -> Splitter.ItemPriceAndCount(20, 1),
          "beans" -> Splitter.ItemPriceAndCount(2, 2)
        ), 30.86),
        "vlad" -> Splitter.PersonTotalOwing(Map(
          "corn" -> Splitter.ItemPriceAndCount(1.5, 1),
          "burrito" -> Splitter.ItemPriceAndCount(13, 1),
          "beans" -> Splitter.ItemPriceAndCount(2, 1)
        ), 19.97)
      ))
    }
    "split correctly among 3 people" in {
      Splitter.split(
        Map(
          "corn" -> Splitter.ItemPriceAndCount(1.5, 2),
          "steak" -> Splitter.ItemPriceAndCount(20, 1),
          "beans" -> Splitter.ItemPriceAndCount(2, 3),
          "burrito" -> Splitter.ItemPriceAndCount(13, 1),
          "soup of the day" -> Splitter.ItemPriceAndCount(15, 1)
        ),
        Map(
          "antonio" -> Seq(
            Splitter.PersonItemCounts("corn", 1),
            Splitter.PersonItemCounts("steak", 1),
            Splitter.PersonItemCounts("beans", 2)
          ),
          "vlad" -> Seq(
            Splitter.PersonItemCounts("corn", 1),
            Splitter.PersonItemCounts("burrito", 1),
            Splitter.PersonItemCounts("beans", 1)
          ),
          "abi" -> Seq(
            Splitter.PersonItemCounts("soup of the day", 1)
          )
        ),
        0.1,
        0.1
      ) should equal(Map(
        "antonio" -> Splitter.PersonTotalOwing(Map(
          "corn" -> Splitter.ItemPriceAndCount(1.5, 1),
          "steak" -> Splitter.ItemPriceAndCount(20, 1),
          "beans" -> Splitter.ItemPriceAndCount(2, 2)
        ), 30.86),
        "vlad" -> Splitter.PersonTotalOwing(Map(
          "corn" -> Splitter.ItemPriceAndCount(1.5, 1),
          "burrito" -> Splitter.ItemPriceAndCount(13, 1),
          "beans" -> Splitter.ItemPriceAndCount(2, 1)
        ), 19.97),
        "abi" -> Splitter.PersonTotalOwing(Map(
          "soup of the day" -> Splitter.ItemPriceAndCount(15, 1)
        ), 18.15)
      ))
    }
    "split correctly among 5 people" in {
      Splitter.split(
        Map(
          "corn" -> Splitter.ItemPriceAndCount(1.5, 2),
          "steak" -> Splitter.ItemPriceAndCount(20, 1),
          "beans" -> Splitter.ItemPriceAndCount(2, 3),
          "burrito" -> Splitter.ItemPriceAndCount(13, 1),
          "soup of the day" -> Splitter.ItemPriceAndCount(15, 3),
          "quasedilla" -> Splitter.ItemPriceAndCount(12, 2),
          "kombucha" -> Splitter.ItemPriceAndCount(4, 1)
        ),
        Map(
          "antonio" -> Seq(
            Splitter.PersonItemCounts("corn", 1),
            Splitter.PersonItemCounts("steak", 1),
            Splitter.PersonItemCounts("beans", 2)
          ),
          "vlad" -> Seq(
            Splitter.PersonItemCounts("corn", 1),
            Splitter.PersonItemCounts("burrito", 1),
            Splitter.PersonItemCounts("beans", 1)
          ),
          "abi" -> Seq(
            Splitter.PersonItemCounts("soup of the day", 1)
          ),
          "alex" -> Seq(
            Splitter.PersonItemCounts("soup of the day", 1),
            Splitter.PersonItemCounts("quasedilla", 1)
          ),
          "justen" -> Seq(
            Splitter.PersonItemCounts("soup of the day", 1),
            Splitter.PersonItemCounts("quasedilla", 1),
            Splitter.PersonItemCounts("kombucha", 1)
          )
        ),
        0.1,
        0.1
      ) should equal(Map(
        "antonio" -> Splitter.PersonTotalOwing(Map(
          "corn" -> Splitter.ItemPriceAndCount(1.5, 1),
          "steak" -> Splitter.ItemPriceAndCount(20, 1),
          "beans" -> Splitter.ItemPriceAndCount(2, 2)
        ), 30.86),
        "vlad" -> Splitter.PersonTotalOwing(Map(
          "corn" -> Splitter.ItemPriceAndCount(1.5, 1),
          "burrito" -> Splitter.ItemPriceAndCount(13, 1),
          "beans" -> Splitter.ItemPriceAndCount(2, 1)
        ), 19.97),
        "abi" -> Splitter.PersonTotalOwing(Map(
          "soup of the day" -> Splitter.ItemPriceAndCount(15, 1)
        ), 18.15),
        "alex" -> Splitter.PersonTotalOwing(Map(
          "soup of the day" -> Splitter.ItemPriceAndCount(15, 1),
          "quasedilla" -> Splitter.ItemPriceAndCount(12, 1)
        ), 32.67),
        "justen" -> Splitter.PersonTotalOwing(Map(
          "soup of the day" -> Splitter.ItemPriceAndCount(15, 1),
          "quasedilla" -> Splitter.ItemPriceAndCount(12, 1),
          "kombucha" -> Splitter.ItemPriceAndCount(4, 1)
        ), 37.51)
      ))
    }
    "errors when there are items allocated to people, but not in items" in {
      assertThrows[Exception] {
        Splitter.split(
          Map(
            "corn" -> Splitter.ItemPriceAndCount(1.5, 2),
            "steak" -> Splitter.ItemPriceAndCount(20, 1),
            "beans" -> Splitter.ItemPriceAndCount(2, 3),
            "burrito" -> Splitter.ItemPriceAndCount(13, 1)
          ),
          Map(
            "antonio" -> Seq(
              Splitter.PersonItemCounts("corn", 1),
              Splitter.PersonItemCounts("steak", 1),
              Splitter.PersonItemCounts("beans", 2)
            ),
            "vlad" -> Seq(
              Splitter.PersonItemCounts("corn", 1),
              Splitter.PersonItemCounts("burrito", 1),
              Splitter.PersonItemCounts("beans", 1)
            )
          ),
          0.1,
          0.1
        ) should equal(Map(
          "antonio" -> Splitter.PersonTotalOwing(Map(
            "corn" -> Splitter.ItemPriceAndCount(1.5, 1),
            "steak" -> Splitter.ItemPriceAndCount(20, 1),
            "beans" -> Splitter.ItemPriceAndCount(2, 2)
          ), 30.86),
          "vlad" -> Splitter.PersonTotalOwing(Map(
            "corn" -> Splitter.ItemPriceAndCount(1.5, 1),
            "burrito" -> Splitter.ItemPriceAndCount(13, 1),
            "beans" -> Splitter.ItemPriceAndCount(2, 1)
          ), 19.97),
          "abi" -> Splitter.PersonTotalOwing(Map(
            "soup of the day" -> Splitter.ItemPriceAndCount(15, 1)
          ), 18.15)
        ))
      }
    }
    "errors when there are unallocated items among people" in {
      assertThrows[Exception] {
        Splitter.split(
          Map(
            "corn" -> Splitter.ItemPriceAndCount(1.5, 2),
            "steak" -> Splitter.ItemPriceAndCount(20, 1),
            "beans" -> Splitter.ItemPriceAndCount(2, 3),
            "burrito" -> Splitter.ItemPriceAndCount(13, 1),
            "soup of the day" -> Splitter.ItemPriceAndCount(15, 1)
          ),
          Map(
            "antonio" -> Seq(
              Splitter.PersonItemCounts("corn", 1),
              Splitter.PersonItemCounts("steak", 1),
              Splitter.PersonItemCounts("beans", 2)
            ),
            "vlad" -> Seq(
              Splitter.PersonItemCounts("corn", 1),
              Splitter.PersonItemCounts("burrito", 1),
              Splitter.PersonItemCounts("beans", 1)
            )
          ),
          0.1,
          0.1
        ) should equal(Map(
          "antonio" -> Splitter.PersonTotalOwing(Map(
            "corn" -> Splitter.ItemPriceAndCount(1.5, 1),
            "steak" -> Splitter.ItemPriceAndCount(20, 1),
            "beans" -> Splitter.ItemPriceAndCount(2, 2)
          ), 30.86),
          "vlad" -> Splitter.PersonTotalOwing(Map(
            "corn" -> Splitter.ItemPriceAndCount(1.5, 1),
            "burrito" -> Splitter.ItemPriceAndCount(13, 1),
            "beans" -> Splitter.ItemPriceAndCount(2, 1)
          ), 19.97)
        ))
      }
    }
  }
}

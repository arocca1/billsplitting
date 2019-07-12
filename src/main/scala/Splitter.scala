object Splitter {
  case class ItemPriceAndCount(price: Double, itemCount: Int)
  case class PersonItemCounts(itemName: String, count: Int)
  case class PersonTotalOwing(itemsAndCounts: Map[String, ItemPriceAndCount], amountOwed: Double)
  def split(items: Map[String, ItemPriceAndCount], personItems: Map[String, Seq[PersonItemCounts]], tipPercentage: Double, taxRate: Double): Map[String, PersonTotalOwing] = {
    if (items.isEmpty && personItems.isEmpty) {
      return Map.empty[String, PersonTotalOwing]
    }
    if (tipPercentage < 0 || tipPercentage > 1) {
      throw new Exception("Tip percentage must be a positive number between 0 and 1")
    }
    if (taxRate < 0 || taxRate > 1) {
      throw new Exception("Tax rate must be a positive number between 0 and 1")
    }

    // need to make sure that number of items exactly matches the allocates items
    val (remainingItems, itemizedSplits) = personItems.foldLeft((items, Map.empty[String, PersonTotalOwing])) { case ((remainingItems, personSplits), (name, itemsForPerson)) =>
      // iterate over each of the items the person has allocated for themselves
      val (itemSplits, totalPersonSplit, simplifiedRemainingItems) = itemsForPerson.foldLeft((Map.empty[String, ItemPriceAndCount], 0: Double, remainingItems)) { case ((itemsAndPricesForPerson, personSplit, currRemainingItems), personItemCount) =>
        val itemName = personItemCount.itemName
        val count = personItemCount.count
        currRemainingItems.get(itemName) match {
          case Some(itemPriceAndCount) => {
            val remainingAllocation = itemPriceAndCount.itemCount - count
            // add to person split
            val newTotal = personSplit + itemPriceAndCount.price * count * (1 + taxRate) * (1 + tipPercentage)
            // handle remaining items count (if 0, then remove)
            // if less than 0, we actually want to include it as well because we can error out later
            val newRemainingItems = if (remainingAllocation == 0) currRemainingItems - itemName else currRemainingItems + (itemName -> ItemPriceAndCount(itemPriceAndCount.price, remainingAllocation))
            (itemsAndPricesForPerson + (itemName -> ItemPriceAndCount(itemPriceAndCount.price, count)), newTotal, newRemainingItems)
          }
          case None => (itemsAndPricesForPerson, personSplit, currRemainingItems + (itemName -> ItemPriceAndCount(0, count)))
        }
      }
      (simplifiedRemainingItems, personSplits + (name -> PersonTotalOwing(itemSplits, math.round(totalPersonSplit * 100) / 100.0)))
    }

    if (!remainingItems.isEmpty) {
      throw new Exception("Itemization be wrong") // TODO make this error more meaningful
    }

    itemizedSplits
  }
}
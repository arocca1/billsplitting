object Splitter {
  def split(items: Map[String, (Double, Int)], personItems: Map[String, Seq[(String, Int)]], tipPercentage: Double, taxRate: Double): Map[String, (Map[String, (Double, Int)], Double)] = {
    if (items.isEmpty || personItems.isEmpty) {
      return Map.empty[String, (Map[String, (Double, Int)], Double)]
    }
    if (tipPercentage < 0) {
      throw new Exception("Tip percentage must be a positive number")
    }
    if (taxRate < 0) {
      throw new Exception("Tax rate must be a positive number")
    }

    // need to make sure that number of items exactly matches the allocates items
    val (remainingItems, itemizedSplits) = personItems.foldLeft((items, Map.empty[String, (Map[String, (Double, Int)], Double)])) { case ((remainingItems, personSplits), (name, itemsForPerson)) =>
      // iterate over each of the items the person has allocated for themselves
      val (itemSplits, totalPersonSplit, simplifiedRemainingItems) = itemsForPerson.foldLeft((Map.empty[String, (Double, Int)], 0: Double, remainingItems)) { case ((itemsAndPricesForPerson, personSplit, currRemainingItems), (itemName, count)) =>
        currRemainingItems(itemName) match {
          case (priceForItem, allocatedForItem) => {
            val remainingAllocation = allocatedForItem - count
            // add to person split
            val newTotal = personSplit + priceForItem * count * taxRate * tipPercentage
            // handle remaining items count (if 0, then remove)
            // if less than 0, we actually want to include it as well because we can error out later
            val newRemainingItems = if (remainingAllocation == 0) currRemainingItems - itemName else currRemainingItems + (itemName -> (priceForItem, remainingAllocation))
            (itemsAndPricesForPerson + (itemName -> (priceForItem, count)), newTotal, newRemainingItems)
          }
          case (_, _) => {
            // the item was never or over-allocated / doesn't exist???
            val itemCount = currRemainingItems(itemName) match {
              case (_, matchedCount) => matchedCount - count
              case (_, _) => -count
            }
            (itemsAndPricesForPerson, personSplit, currRemainingItems + (itemName -> (0, itemCount)))
          }
        }
      }
      (simplifiedRemainingItems, personSplits + (name -> (itemSplits, totalPersonSplit)))
    }

    if (!remainingItems.isEmpty) {
      throw new Exception("Itemization be wrong") // TODO make this error more meaningful
    }

    itemizedSplits
  }
}
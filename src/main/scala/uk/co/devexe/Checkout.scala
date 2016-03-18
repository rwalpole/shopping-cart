package uk.co.devexe

/**
 *  A simple checkout program that takes a list of items and calculates the total cost
 *  after applying discount offers
 *
 * Author: Rob Walpole (robkwalpole@gmail.com)
 * Date: 18/3/2016
 *
 */
object Checkout {
    def main(args: Array[String]): Unit = {
        val checkout = new Checkout
        System.out.println("Total cost: £" + checkout.getTotal(args.toList))
    }
}

class Checkout {

    /** Takes an item name and returns the cost for that item
      *
      * @param item
      * @return
      */
    def getItemCost(item: String): Double = {
        item match {
            case "apple" => 0.6
            case "orange" => 0.25
            case _ => 0 //TODO - what should happen if the item is unknown?
        }
    }

    /** Takes a map of item name -> item count and returns a map of item name -> item total cost
      *
      * @param itemMap
      * @return
      */
    def getCostMap(itemMap: Map[String,Int]): Map[String,Double] = {
        itemMap map { item =>
            val cost = getItemCost(item._1)
            val itemTotal = cost * item._2
            item._1 -> itemTotal
        }
    }

    /** Takes a map of item name -> item count and returns a map with none-chargeable items removed
      *
      * @param itemMap
      * @return
      */
    def applyOffers(itemMap: Map[String,Int]): Map[String,Int] = {
        val appleOffer = ThreeForThePriceOfTwoOffer("apple")
        val orangeOffer = BuyOneGetOneFreeOffer("orange")
        appleOffer.apply(
            orangeOffer.apply(itemMap)
        )
    }

    /** Takes a list of item names and returns a total item cost after applying discount offers
      * This is the main method access point to the program
      *
      * @param items
      * @return
      */
    def getTotal(items: List[String]): Double = {
        val groupedItems = items.groupBy(name => name).mapValues(_.size)
        val groupedItems1 = applyOffers(groupedItems)
        val costMap = getCostMap(groupedItems1)
        costMap.foldLeft(0.0)((allItemTotal, item) => allItemTotal + item._2)
    }

}

trait Offer {

    /** Takes a map of item name -> item count and applies the relevant offer, removing any items which are not chargeable
      *
      * @param itemMap
      * @return
      */
    def apply(itemMap: Map[String,Int]): Map[String,Int]
}

case class ThreeForThePriceOfTwoOffer(item: String) extends Offer {

    def apply(itemMap: Map[String,Int]): Map[String,Int] = {
        val itemCount = itemMap.get(item) match {
            case Some(x) => x
            case None => 0
        }
        val itemsToDiscount = itemCount / 3
        itemMap - item + (item -> (itemCount - itemsToDiscount))
    }
}

case class BuyOneGetOneFreeOffer(item: String) extends Offer {

    def apply(itemMap: Map[String,Int]): Map[String,Int] = {
        val itemCount = itemMap.get(item) match {
            case Some(x) => x
            case None => 0
        }
        val itemsToDiscount = itemCount / 2
        itemMap - item + (item -> (itemCount - itemsToDiscount))
    }
}
package uk.co.devexe

import org.junit.Assert._
import org.junit._

class CheckoutTest {


    @Test
    def testGetItemCost {
        val checkout = new Checkout
        assertEquals(0.6,checkout.getItemCost("apple"),0)
        assertEquals(0.25,checkout.getItemCost("orange"),0)
    }

    @Test
    def testGetTotal {
        val checkout = new Checkout
        assertEquals(1.45,checkout.getTotal(List("apple","apple","orange")),0)
    }

    @Test
    def testGetCostMap {
        val checkout = new Checkout
        val result = checkout.getCostMap(Map("apple" -> 2, "orange" -> 1))
        assertEquals(1.2,result.get("apple").get,0)
        assertEquals(0.25,result.get("orange").get,0)
    }

    @Test
    def testApplyOffers {
        val checkout = new Checkout
        val itemMap = Map("apple" -> 3, "orange" -> 2)
        val result = checkout.applyOffers(itemMap)
        assertEquals(2,result.get("apple").get,0)
        assertEquals(1,result.get("orange").get,0)
    }

}



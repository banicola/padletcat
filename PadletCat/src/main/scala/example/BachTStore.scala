package example

/* --------------------------------------------------------------------------

   The BachT store


   AUTHOR : J.-M. Jacquet and D. Darquennes
   DATE   : March 2016

----------------------------------------------------------------------------*/

import scala.collection.mutable.Map
import scala.swing._

class BachTStore {

  var theStore = Map[String,(String, String)]()

  def tell(token:String, title: String, content: String):Boolean = {
    if (theStore.contains(token))
    { false}
    else
    {
      theStore = theStore ++ Map(token -> (title, content))
      true
    }
  }

  def isEmpty():Boolean = theStore.isEmpty

  def ask(token:String):Boolean = {
    theStore.contains(token)
  }


  def get(token:String):Boolean = {
    if (theStore.contains(token)) {
      theStore.remove(token)
      true
    }
    else false
  }


  def nask(token:String):Boolean = {
    !theStore.contains(token)
  }

  def print_store {
    print("{ ")
    for ((t,d) <- theStore)
      print ( t + "(" + theStore(t) + ")" )
    println(" }")
  }

  def clear_store {
    theStore = Map[String,(String, String)]()
  }

}

object bb extends BachTStore {

  def reset { clear_store }

}
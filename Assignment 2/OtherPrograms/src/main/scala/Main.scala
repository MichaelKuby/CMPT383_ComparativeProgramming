import scala.collection.parallel.CollectionConverters._
import math.Ordered.orderingToOrdered
import scala.util.Random
import java.lang.String

/* NOTE: FOR THIS TO RUN YOU WILL NEED TO CHANGE THE build.sbt FILE. Please
add the following to library dependencies:

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
  
*/

/* The following is a quick and easy quicksort implementation that can be used
to sort any orderable collection in Scala. This is a pure function
that makes use of Scala's flexibility. 

Help creating the signature from here: https://www.baeldung.com/scala/sorting
*/
def quicksort[A](xs: List[A])(implicit ord: Ordering[A]): List[A] = xs match
  case Nil => Nil
  case x :: xs => 
    val smaller = xs.filter(elem => elem <= x)
    val larger = xs.filter(elem => elem > x)
    quicksort(smaller) ++ List(x) ++ quicksort(larger)


/* The following shows off Scala's OOP capabilities. First we implement an 
abstract class called MagicalCreature. Let's suppose all magical creatures have
a name, an attack, and a method of travel, so we define those common attributes */
abstract class MagicalCreature {
  val typeOf: String
  val attack: Double
  val travel: String
  def displayInfo(): Unit = {
    println(s"Hello, I am of type $typeOf.")
    println(s"I do damage of " + attack)
    println(s"$typeOf's travel by " + travel + "\n")
  }
}

/* Now we define specific classes: Dragons and Unicorns! */
class Dragon() extends MagicalCreature {
  val typeOf = "Dragon"
  val attack = 100
  val travel = "soaring through the sky with mighty wings."
}

class Unicorn() extends MagicalCreature {
  val typeOf = "Unicorn"
  val attack = 20
  val travel = "prancing through fields."
}

class Fairy() extends MagicalCreature {
  val typeOf = "Fairy"
  val attack = 5
  val travel = "fluttering about, aimlessly."
}

/* randomCreatures makes use of the tabulate function. Tabulate has syntax
List.tabulate(size)(f) where size is the number of elements in the resulting
list (interestingly this can create multi-dimensional lists). f is a function
taking an index as its argument and produces al element for that index.

randomCreatures creates n creatures, each time randomly deciding based on a
randomly created integer from 0 to 2 

Insight into how to use tabulate from here: 
  https://www.educative.io/answers/what-is-tabulate-in-scala
*/
def randomCreatures(n: Int): List[MagicalCreature] = List.tabulate(n) ( _ =>
    val random = Random.nextInt(3)
    if random == 0 then new Dragon()
    else if random == 1 then new Unicorn()
    else new Fairy()
)

/* sumDmgParallel makes use of the scala.collection.parallel library

First we convert our list to a parallelizable collection using .par
Then we invoke the aggregate function, which takes two functions
as arguments. 

The first function is the "combining function",
since it stipulates an accumulator variable and the characteristic 
to sum over.

The second function is the merging function, taking the partial
results and combining them. */

def sumDmgParallel(creatures: List[MagicalCreature]): Double =
  creatures.par.aggregate(0.0)(
    (acc, creatures) => acc + creatures.attack,
    (acc1, acc2) => acc1 + acc2
  )

  /* NOTE: In testing it seems that parallelizing the sum of the damage
  does not noticeably improve performance. The function randomCreatures that 
  creates the objects to sum over is the real bottleneck here, and if you are
  reading this, it probably means I didn't have time to amend that function
  to speed it up as well. 
  
  Nevertheless, in a real world scenario where data is collected over time 
  sumDmgParallel could prove useful */

@main def main(): Unit =

  // Test the quicksort function

  val random = new Random()
  val size = 20
  val randomList = List.fill(size)(random.nextInt(100))
  println("Unordered list: " + randomList + "\n")

  val sortedList = quicksort(randomList)
  println("Sorted list: " + sortedList + "\n")
    
  // Test the classes

  val dragon = Dragon()
  val unicorn = Unicorn()
  val fairy = Fairy()
  dragon.displayInfo()
  unicorn.displayInfo()
  fairy.displayInfo()

  // Test sumDmgParallel function
  
  val n = 100000
  val nCreatures = randomCreatures(n)

  // Compute and print their cumulative damage

  val totalDamage = sumDmgParallel(nCreatures)
  println("The total accumulated damage at the disposal of these formidable creatures: " + totalDamage)
import scala.math

@main def main: Unit =
    // Test divisors function
    val n = 10
    println(divisors(n))

    // Test primes function
    println(primes(n))

    // Test pythagorean function
    println(pythagorean(30))

    // Test merge function
    val list1 = List(1, 3, 5)
    val list2 = List(2, 4, 6)
    val mergedList = merge(list1, list2)
    println(mergedList)

    // Test mergeSort function
    val unsortedList = List(9, 4, 7, 2, 1, 5, 3, 6, 8)
    val sortedList = mergeSort(unsortedList)
    println(sortedList)

def divisors(n: Int): List[Int] =
  val numbers = (2 to n / 2).toList
  for num <- numbers if n % num == 0
  yield num

def primes(n: Int): List[Int] =
  val numbers = (2 to n).toList
  for num <- numbers if divisors(num).isEmpty
  yield num

def pythagorean(n: Int): List[(Int, Int, Int)] = 
  
  def pythagoreanHelper(a: Int, b: Int, c: Int): Boolean =
  (math.pow(a, 2) + math.pow(b, 2) == math.pow(c, 2)) == true
  
  val triples = for {
    c <- 1 to n
    b <- 1 until c
    a <- 1 until b
    if pythagoreanHelper(a, b, c)
  } yield (a,b,c)

  triples.toList // otherwise this will return an IndexedSeq (superclass of lists, vectors, etc.)

def merge[A](xs: List[A], ys: List[A])(implicit ord: Ordering[A]): List[A] = (xs, ys) match 
  case (Nil, Nil) => Nil
  case (xs, Nil) => xs
  case (Nil, ys) => ys
  case (x :: xs, y :: ys) => // match the patterns of two non-empty lists
    if (ord.lteq(x,y)) x :: merge(xs, y :: ys) // x <= y, so append x to the head and recurse
    else y :: merge (x :: xs, ys) // otherwise append y and recurse

def mergeSort[A](xs: List[A])(implicit ord: Ordering[A]): List[A] = xs match
  case Nil => Nil           // empty list
  case List(x) => List(x)   // one element list
  case xs =>                // 2+ element list
    val (leftSide, rightSide) = splitInTwo(xs)  // split the list in two
    merge (mergeSort(leftSide), mergeSort(rightSide))

  def splitInTwo[A](xs: List[A]): (List[A], List[A]) = 
    val midpoint = xs.length / 2  // get the midpoint
    xs.splitAt(midpoint)          // split at the midpoint
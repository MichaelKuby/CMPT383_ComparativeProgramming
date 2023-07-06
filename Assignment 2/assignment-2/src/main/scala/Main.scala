import scala.math
import java.time.LocalDate
import java.time.format.DateTimeFormatter

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

    // Test isFriday function
    val dateToCheck = LocalDate.of(1988, 3, 1)     // My year and date of birth. Should be a Tuesday.
    val isFriday1988 = isFriday(dateToCheck)  
    println("Is March 1, 1988, a Friday? " + isFriday1988)

    val dateToCheck2 = LocalDate.of(1988, 3, 4)     // Should be a Friday
    val isFriday19882 = isFriday(dateToCheck2)       
    println("Is March 4, 1985, a Friday? " + isFriday19882)

    // Test isPrime function
    val x = 4
    println(isPrime(x))

    // Test isPrimeDay function
    val testDates = List(
      LocalDate.of(2023, 7, 2),   // Prime day
      LocalDate.of(2023, 7, 3),   // Not prime day
      LocalDate.of(2023, 7, 5),   // Prime day
      LocalDate.of(2023, 7, 8)    // Not prime day
    )

    testDates.foreach(date => {
    val isPrimeDayResult = isPrimeDay(date)
      println(s"${date} is a prime day: " + isPrimeDayResult)
      })

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

  // isFriday takes a Java LocalDate object and 
  // returns true if the given date is a Friday
  def isFriday(date: LocalDate): Boolean = 
    val formatter = DateTimeFormatter.ofPattern("EEEE")   // Requests the full name of the week
    val dayToString = date.format(formatter)  // extract the day as string
    if dayToString == "Friday" then true else false

  def isPrime(n: Int): Boolean = 
    if n == 1 then false
    else if divisors(n) == Nil then true 
    else false

  def isPrimeDay(date: LocalDate): Boolean =
    val dayOfMonth = date.getDayOfMonth()
    isPrime(dayOfMonth)
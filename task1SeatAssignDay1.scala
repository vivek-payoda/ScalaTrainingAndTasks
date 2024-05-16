import scala.util.control.Breaks._
import scala.io.StdIn



def printAll(allSeats : Array[Array[String]]) : Unit = {
  val row = allSeats.length
  val col = allSeats(0).length
  for( i <- 0 to row-1)
  {for( j <- 0 to col-1)
  {
    print(allSeats(i)(j) + " ")
    if(j == col - 1) println(" ")
  }}


}




def assignSeats(arr: Array[Array[String]], seatNumber: Array[Int], callBack : Array[Array[String]] => Unit) : Unit = {

  println("initial seat array given ")

  callBack(arr);
  val n = arr.length
  val length = seatNumber.length
  for(i <- 0 to length-1)
  {
    val x = seatNumber(i)/n;
    val y = seatNumber(i)%n -1;

    arr(x)(y) = "X"
  }
  println("-------------------------")
  println("modified seat array is below ")
  callBack(arr)

}

@main
def runner() = {

  val arr = Array.fill(10, 10)("1")
  arr(0)(0) = "X"
  arr(2)(0) = "X"
  arr(0)(3) = "X"

  var userInput = true;

  breakable {
    while (userInput) {
      println("give a string of seat to book");
      val line = StdIn.readLine("Enter a string: ")
      val nums = line.split(" ").map(_.toInt)

      assignSeats(arr, nums, printAll)

      val conti = StdIn.readLine("should we continue? ")
      if (conti == "false") break()

    }
  }

}
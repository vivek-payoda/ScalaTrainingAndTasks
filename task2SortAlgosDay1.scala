import scala.util.control.Breaks._
import scala.io.StdIn

def findPivot(arr : Array[Int], l : Int, h : Int): Int = {
  var low = l
  val piv = arr(h)
  for( i <- l until h){
     if(arr(i) <= piv) {
       val temp = arr(low)
       arr(low) = arr(i)
       arr(i) = temp
       low = low +1
     }
  }
  val temp = arr(low)
  arr(low) = arr(h)
  arr(h) = temp
  low
}

def quick(arr : Array[Int], l : Int, h : Int) : Unit = {
    if(l <=h) {

      val pivot = findPivot(arr, l , h)
      quick(arr, l , pivot -1)
      quick(arr, pivot+1, h)
    }
}


def mergeSort(arr: Array[Int]): Unit = {
  def mergeSortHelper(start: Int, end: Int): Unit = {
    if (start < end) {
      val mid = (start + end) / 2
      mergeSortHelper(start, mid)
      mergeSortHelper(mid + 1, end)
      merge(start, mid, end)
    }
  }

  def merge(start: Int, mid: Int, end: Int): Unit = {
    val left = arr.slice(start, mid + 1)
    val right = arr.slice(mid + 1, end + 1)
    var i = 0
    var j = 0
    var k = start

    while (i < left.length && j < right.length) {
      if (left(i) <= right(j)) {
        arr(k) = left(i)
        i += 1
      } else {
        arr(k) = right(j)
        j += 1
      }
      k += 1
    }

    while (i < left.length) {
      arr(k) = left(i)
      i += 1
      k += 1
    }

    while (j < right.length) {
      arr(k) = right(j)
      j += 1
      k += 1
    }
  }

  mergeSortHelper(0, arr.length - 1)
}

def binarySearch(arr : Array[Int], target: Int) : Int = {
  var l = 0
  var h = arr.length -1
  var mid = 0
  var ans = 0
  breakable {
    while (l <= h) {
      mid = l + (h - l) / 2
      if (arr(mid) == target) {
        ans = mid
        break()
      }
      if(arr(mid) < target) l = mid +1
      else h = mid -1

    }
  }
  ans
}

def getAlgorithm(algoType : String): Either[Array[Int] => Unit, (Array[Int], Int) => Int] = {
  algoType match {
    case "bubble" => Left((arr: Array[Int]) => {
      val n = arr.length
      for (i <- 0 until n - 1) {
        for (j <- 0 until n - i - 1) {
          if (arr(j) > arr(j + 1)) {

            val temp = arr(j)
            arr(j) = arr(j + 1)
            arr(j + 1) = temp
          }
        }
      }
    })
    case "insertion" => Left((arr: Array[Int]) => {
      for (i <- 1 until arr.length) {
        val key = arr(i)
        var j = i - 1
        while (j >= 0 && arr(j) > key) {
          arr(j + 1) = arr(j)
          j = j - 1
        }
        arr(j + 1) = key
      }
    })
    case "quick" => Left((arr: Array[Int]) => {
       quick(arr, 0, arr.length - 1)
    })
    case "merge" => Left((arr: Array[Int]) => {
      mergeSort(arr)
    })
    case "binarySearch" => Right((arr: Array[Int], target: Int) => binarySearch(arr, target))
  }
}



@main
def task2(): Unit = {
    val arr = Array(2,33,5,10)
    println("write the sort function name you want to use");
    val line = StdIn.readLine("Enter the sort name: ")
    println(s"sort to be used is $line")
    val sorting = getAlgorithm(line)

    sorting match {
      case Left(sort) => {
        println("sort is here")
        println(arr.mkString(" "))
        sort(arr)
        println(arr.mkString(" "))
      }
      case Right(_) => println("no sort present")
    }
    val searching = getAlgorithm("binarySearch")
    searching match {
      case Right(search) => {
        println("search the hell out of it")
        val ind = search(arr, 5)
        println(s"the index is $ind")
      }
      case Left(_) => println("no search present")
    }
//    insertion(arr)
//
//
//    arr = Array(2,33,55,31,10)
//    val bubble = getAlgorithm("bubble")
//    bubble(arr)
//    println("Bubble sort is here")
//    println(arr.mkString(" "))
//
//    arr = Array(102,2,35,55,11)
//    val quick = getAlgorithm("quick")
//    quick(arr)
//    println("quick sort is here")
//    println(arr.mkString(" "))
//
//    arr = Array(104, 2, 35, 5, 11)
//    val merge = getAlgorithm("merge")
//    merge(arr)
//    println("merge sort is here")
//    println(arr.mkString(" "))



}
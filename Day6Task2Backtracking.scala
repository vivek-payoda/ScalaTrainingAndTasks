
import scala.io.Source
import scala.collection.mutable._

@main
def day6ParallelProcess(): Unit = {

  val dataPath = "/Users/vivekupadhyay/IdeaProjects/ScalaTraining1/src/main/scala-3/employees.csv"

  val fileSource = Source.fromFile(dataPath)
  var salesEmployeeList: List[String] = List()

  var lineIndex: Int = 0
  for (line <- fileSource.getLines()) {
    if (lineIndex > 0) {
      val Array(id, fullName, location, income, division) = line.split(",").map(_.trim)
      if(division == "Sales")
        salesEmployeeList = salesEmployeeList :+ s"-Name : $fullName, Location : $location-"
    }
    lineIndex += 1
  }

  var salesPersonList : ArrayBuffer[String] = ArrayBuffer()

  backTrack(salesPersonList, salesEmployeeList, 0, salesEmployeeList.length)


}


def backTrack(salesPersonList : ArrayBuffer[String] , salesEmployeeList: List[String], start: Int, n: Int) : Unit = {
   if(salesPersonList.length == 4) {
     println("the 4 person from sales")
     println(salesPersonList.mkString(" "))
      println("----------")
      return
   }

  for( i<- start until n){
    salesPersonList += salesEmployeeList(i)
    backTrack(salesPersonList, salesEmployeeList, i+1, n)
    salesPersonList.remove(salesPersonList.length - 1)
  }
}

case class Staff(id: Int, fullName: String, location: String, income: Double, department: String)
case class DepartmentTask(departmentName: String)



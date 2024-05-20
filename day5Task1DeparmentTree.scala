
import scala.io.Source
import scala.collection.mutable
import java.sql.{Connection, DriverManager, ResultSet, Statement}

@main
def departmentCode(): Unit = {

  val dataPath = "/Users/vivekupadhyay/IdeaProjects/ScalaTraining1/src/main/scala-3/employees.csv"

  val fileSource = Source.fromFile(dataPath)
  var employeeList: List[Staff] = List()
  var uniqueDepartments: mutable.Set[String] = mutable.Set()
  var departmentMap: mutable.Map[String, mutable.ArrayBuffer[Staff]] = mutable.Map()

  var lineIndex: Int = 0
  for (line <- fileSource.getLines()) {
    if (lineIndex > 0) {
      val Array(id, fullName, location, income, division) = line.split(",").map(_.trim)
      val staffMember: Staff = Staff(id.toInt, fullName, location, income.toDouble, division)
      uniqueDepartments += division
      employeeList = employeeList :+ staffMember
      // Add staffMember to the department map
      if (departmentMap.contains(division)) {
        departmentMap(division) += staffMember
      } else {
        departmentMap(division) = mutable.ArrayBuffer(staffMember)
      }
    }
    lineIndex += 1
  }

  println(uniqueDepartments.mkString(" "))



  Class.forName("com.mysql.cj.jdbc.Driver")

  // Establish a connection
  val url = "jdbc:mysql://hadoop-server.mysql.database.azure.com:3306/vivekupadhyay"
  val username = "sqladmin"
  val password = "Password@12345"
  val connection: Connection = DriverManager.getConnection(url, username, password)

  try {
    // Create a statement
    val statement: Statement = connection.createStatement()

    val createDepartmentSQL =
      """
        |CREATE TABLE IF NOT EXISTS Deprtmnt (
        |  id INT AUTO_INCREMENT PRIMARY KEY,
        |  departmentName VARCHAR(100) UNIQUE
        |)
        |""".stripMargin

    statement.execute(createDepartmentSQL)


    val createEmployeeSQL =
      """
        |CREATE TABLE IF NOT EXISTS Employ (
        |  id INT PRIMARY KEY,
        |  fullName VARCHAR(100),
        |  location VARCHAR(100),
        |  income DOUBLE,
        |  departName VARCHAR(100),
        |  FOREIGN KEY (departName) REFERENCES Deprtmnt(departmentName)
        |)
        |""".stripMargin

    statement.execute(createEmployeeSQL)

    // Insert unique departments into Deprtmnt table
    val insertDepartmentSQL = "INSERT INTO Deprtmnt (departmentName) VALUES (?)"
    val departmentPreparedStatement = connection.prepareStatement(insertDepartmentSQL)

    uniqueDepartments.foreach { department =>
      departmentPreparedStatement.setString(1, department)
      departmentPreparedStatement.executeUpdate()
    }

    // Insert employee data into Employ table
    val insertEmployeeSQL = "INSERT INTO Employ (id, fullName, location, income, departName) VALUES (?, ?, ?, ?, ?)"
    val employeePreparedStatement = connection.prepareStatement(insertEmployeeSQL)

    employeeList.foreach { employee =>
      employeePreparedStatement.setInt(1, employee.id)
      employeePreparedStatement.setString(2, employee.fullName)
      employeePreparedStatement.setString(3, employee.location)
      employeePreparedStatement.setDouble(4, employee.income)
      employeePreparedStatement.setString(5, employee.department)
      employeePreparedStatement.executeUpdate()
    }

    println("Data insertion completed successfully.")

    // Retrieve and print department and corresponding employee data
    val selectDepartmentsSQL = "SELECT departmentName FROM Deprtmnt"
    val departmentResultSet: ResultSet = statement.executeQuery(selectDepartmentsSQL)

    while (departmentResultSet.next()) {
//      val departmentName = departmentResultSet.getString("departmentName")
//      println(s"-|-- department name : $departmentName")
//      
//      val selectEmp = "select id, fullName, location, income, departName From Employ where departName = ?"
//      val employeePreparedStatement = connection.prepareStatement(selectEmp)
//      employeePreparedStatement.setString(1, departmentName)
//      val employeeSet : ResultSet = employeePreparedStatement.executeQuery()
//      
//      while(employeeSet.next()) {
//        val id 
//      
//      }
      val departmentName = departmentResultSet.getString("departmentName")
      println(s"\n -|-Department: $departmentName")

      val selectEmployeesSQL = "SELECT id, fullName, location, income, departName FROM Employ WHERE departName = ?"
      val employeePreparedStatement = connection.prepareStatement(selectEmployeesSQL)
      employeePreparedStatement.setString(1, departmentName)
      val employeeResultSet: ResultSet = employeePreparedStatement.executeQuery()

      while (employeeResultSet.next()) {
        val id = employeeResultSet.getInt("id")
        val fullName = employeeResultSet.getString("fullName")
        val location = employeeResultSet.getString("location")
        val income = employeeResultSet.getDouble("income")
        println(s"--|------ ID: $id, Name: $fullName, Location: $location, Income: $income")
      }

//    // Print the department map
//    println("\nDepartment Map:")
//    departmentMap.foreach { case (department, staffMembers) =>
//      println(s"Department: $department")
//      staffMembers.foreach { staffMember =>
//        println(s"  ID: ${staffMember.id}, Name: ${staffMember.fullName}, Location: ${staffMember.location}, Income: ${staffMember.income}")
//      }
//    }
}
  }
  catch {
    case e: Exception => e.printStackTrace()
  } finally {
    // Close Statement and Connection
    connection.close()
  }
}

case class Staff(id: Int, fullName: String, location: String, income: Double, department: String)
case class DepartmentTask(departmentName: String)


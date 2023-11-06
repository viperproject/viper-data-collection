package database

import slick.jdbc.MySQLProfile.api._
object DBConnection {
  val db = Database.forConfig("mysql")
}

package database

import slick.jdbc.PostgresProfile.api._

object DBConnection {
  val db = Database.forConfig("postgres")
}

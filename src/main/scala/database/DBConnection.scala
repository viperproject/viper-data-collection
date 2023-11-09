package database

import slick.jdbc.PostgresProfile.api._

/** Object representing a connection to the program database */
object DBConnection {
  val db = Database.forConfig("postgres")
}

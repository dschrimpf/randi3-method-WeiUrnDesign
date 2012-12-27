package org.randi3.schema

import org.scalaquery.ql._
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql.extended.{ExtendedTable => Table}
import org.scalaquery.ql.extended._
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession


/**
 * A simple example that uses statically typed queries against an in-memory
 * H2 database. The example data comes from Oracle's JDBC tutorial at
 * http://download.oracle.com/javase/tutorial/jdbc/basics/tables.html.
 */
class WeiUrnDesignRandomizationSchema(driver: ExtendedProfile) {
  import driver.Implicit._

  val schema = new DatabaseSchema(driver)

  object WeiUrnDesignRandomizations extends Table[(Int, Int, Option[Int], Int, Int)]("WeiUrnDesignRandomization") {
    def id = column[Int]("ID", O PrimaryKey, O AutoInc)

    def version = column[Int]("Version", O NotNull)

    def randomizationMethodId = column[Option[Int]]("RandomizationMethodId")

    def alpha = column[Int]("alpha", O NotNull)

    def beta = column[Int]("beta", O NotNull)

    def * = id ~ version ~ randomizationMethodId ~ alpha ~ beta

    def noId = version ~ randomizationMethodId~ alpha ~ beta

    def randomizationMethod = foreignKey("WeiUrnDesignRandomizationFK_RandomizationMethod", randomizationMethodId, schema.RandomizationMethods)(_.id)
  }

  object WeiUrn extends Table[(Int, Option[Int], Int, String)]("WeiUrn") {
    def id = column[Int]("ID", O PrimaryKey, O AutoInc)

    def randomizationMethodId = column[Option[Int]]("RandomizationMethodId")

    def treatmentArmId = column[Int]("TreatmentArmId")

    def stratum = column[String]("Stratum")

    def * = id ~ randomizationMethodId ~ treatmentArmId ~ stratum

    def noId = randomizationMethodId ~ treatmentArmId ~ stratum

    def randomizationMethod = foreignKey("WeiUrnFK_RandomizationMethod", randomizationMethodId, schema.RandomizationMethods)(_.id)

    def treatmentArm = foreignKey("WeiUrnFK_TreatmentArm", treatmentArmId, schema.TreatmentArms)(_.id)
  }


  def getDatabaseTables: DDL = {
    (WeiUrnDesignRandomizations.ddl ++ WeiUrn.ddl)
  }
}

package org.randi3.utility


import org.randi3.dao._
import org.randi3.schema.{WeiUrnDesignRandomizationSchema, LiquibaseUtil}



object TestingEnvironmentUrn extends TestingEnvironment {

  val schemaUrn = new WeiUrnDesignRandomizationSchema(driver)

  LiquibaseUtil.updateDatabase(database, "db/db.changelog-master-weiUrnDesign.xml", this.getClass.getClassLoader)

  lazy val weiUrnDesignRandomizationDao = new WeiUrnDesignRandomizationDao(database, driver)
}
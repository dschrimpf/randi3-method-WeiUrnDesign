package org.randi3.randomization

import org.randi3.randomization.configuration._
import org.randi3.dao.WeiUrnDesignRandomizationDao
import org.randi3.model._
import org.randi3.model.criterion.Criterion
import org.randi3.model.criterion.constraint.Constraint
import org.scalaquery.ql._
import org.scalaquery.ql.extended.ExtendedProfile
import org.scalaquery.session.Database
import scalaz._

import org.apache.commons.math3.random._

import org.randi3.schema.{LiquibaseUtil, WeiUrnDesignRandomizationSchema}

class WeiUrnDesignRandomizationPlugin(database: Database, driver: ExtendedProfile) extends RandomizationMethodPlugin(database, driver) {


  val schema = new WeiUrnDesignRandomizationSchema(driver)
  import schema._


  val name = classOf[WeiUrnDesignRandomization].getName

  val i18nName = name

  val description = "Wei's urn design algorithm (only for trials with two treatment arms"

  val canBeUsedWithStratification = true

  private val weiUrnDesignRandomizationDao = new WeiUrnDesignRandomizationDao(database, driver)

  private val alphaConfigurationType = new IntegerConfigurationType(name = "alpha", description = "alpha")

  private val betaConfigurationType = new IntegerConfigurationType(name = "beta", description = "beta")


  def randomizationConfigurationOptions(): (List[ConfigurationType[Any]], List[Criterion[_ <: Any, Constraint[_ <: Any]]]) = {
    (List(alphaConfigurationType, betaConfigurationType), Nil)
  }

  def getRandomizationConfigurations(id: Int): List[ConfigurationProperty[Any]] = {
    val method = weiUrnDesignRandomizationDao.get(id).toOption.getOrElse(return Nil).getOrElse(return Nil)
    List(new ConfigurationProperty[Any](alphaConfigurationType, method.alpha), new ConfigurationProperty[Any](betaConfigurationType, method.beta))
  }

  def randomizationMethod(random: RandomGenerator, trial: Trial, configuration: List[ConfigurationProperty[Any]]): Validation[String, RandomizationMethod] = {
    if (configuration.isEmpty) Failure("No configuration available")
    else Success(new WeiUrnDesignRandomization(alpha = configuration(0).value.asInstanceOf[Int], beta = configuration(1).value.asInstanceOf[Int])(random = random))
  }

  def databaseTables(): Option[DDL] = {
    Some(getDatabaseTables)
  }

  def updateDatabase() {
    LiquibaseUtil.updateDatabase(database, "db/db.changelog-master-weiUrnDesign.xml", this.getClass.getClassLoader)
  }

  def create(randomizationMethod: RandomizationMethod, trialId: Int): Validation[String, Int] = {
    weiUrnDesignRandomizationDao.create(randomizationMethod.asInstanceOf[WeiUrnDesignRandomization], trialId)
  }

  def get(id: Int): Validation[String, Option[RandomizationMethod]] = {
    weiUrnDesignRandomizationDao.get(id)
  }

  def getFromTrialId(trialId: Int): Validation[String, Option[RandomizationMethod]] = {
    weiUrnDesignRandomizationDao.getFromTrialId(trialId)
  }

  def update(randomizationMethod: RandomizationMethod): Validation[String, RandomizationMethod] = {
    weiUrnDesignRandomizationDao.update(randomizationMethod.asInstanceOf[WeiUrnDesignRandomization])
  }

  def delete(randomizationMethod: RandomizationMethod) {
    weiUrnDesignRandomizationDao.delete(randomizationMethod.asInstanceOf[WeiUrnDesignRandomization])
  }

}

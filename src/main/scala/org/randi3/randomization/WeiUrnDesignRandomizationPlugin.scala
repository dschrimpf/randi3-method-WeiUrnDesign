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
import org.randi3.utility.{I18NHelper, I18NRandomization, AbstractSecurityUtil}

class WeiUrnDesignRandomizationPlugin(database: Database, driver: ExtendedProfile, securityUtil: AbstractSecurityUtil) extends RandomizationMethodPlugin(database, driver, securityUtil) {

  private val i18n = new I18NRandomization(I18NHelper.getLocalizationMap("urnRandomizationM", getClass.getClassLoader), securityUtil)

  val schema = new WeiUrnDesignRandomizationSchema(driver)
  import schema._


  val name = classOf[WeiUrnDesignRandomization].getName

  def i18nName = i18n.text("name")

  def description = i18n.text("description")

  val canBeUsedWithStratification = true

  private val weiUrnDesignRandomizationDao = new WeiUrnDesignRandomizationDao(database, driver)

  private def alphaConfigurationType = new IntegerConfigurationType(name = i18n.text("alpha"), description =  i18n.text("alphaDesc"))

  private def betaConfigurationType = new IntegerConfigurationType(name =  i18n.text("beta"), description =  i18n.text("betaDesc"))


  def randomizationConfigurationOptions(): (List[ConfigurationType[Any]], Map[String,List[Criterion[_ <: Any, Constraint[_ <: Any]]]]) = {
    (List(alphaConfigurationType, betaConfigurationType), Map())
  }

  def getRandomizationConfigurations(id: Int): List[ConfigurationProperty[Any]] = {
    val method = weiUrnDesignRandomizationDao.get(id).toOption.getOrElse(return Nil).getOrElse(return Nil)
    List(new ConfigurationProperty[Any](alphaConfigurationType, method.alpha), new ConfigurationProperty[Any](betaConfigurationType, method.beta))
  }

  def randomizationMethod(random: RandomGenerator, trial: Trial, configuration: List[ConfigurationProperty[Any]]): Validation[String, RandomizationMethod] = {
    if (configuration.isEmpty) Failure(i18n.text("noConfAvailable"))
      if(trial.treatmentArms.size != 2) Failure(i18n.text("onlyTwoTreatments"))
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

package org.randi3.dao

import scala.slick.session.Database
import scala.slick.session.Database.threadLocalSession
import scala.slick.driver.ExtendedProfile
import scala.slick.lifted._

import org.randi3.randomization.WeiUrnDesignRandomization
import scala.collection.mutable.ListBuffer
import scalaz._
import org.randi3.schema.{DatabaseSchema, WeiUrnDesignRandomizationSchema}

class WeiUrnDesignRandomizationDao(database: Database, driver: ExtendedProfile) extends AbstractRandomizationMethodDao(database, driver) {

  import driver.Implicit._

  val schemaCore = new DatabaseSchema(driver)
  import schemaCore._
  val schemaBlock = new WeiUrnDesignRandomizationSchema(driver)
  import schemaBlock._

  private val queryWeiUrnDesignRandomizationFromId = for {
    id <- Parameters[Int]
    weiUrnDesign <- WeiUrnDesignRandomizations if weiUrnDesign.randomizationMethodId is id
  } yield weiUrnDesign.id ~ weiUrnDesign.version ~ weiUrnDesign.randomizationMethodId ~ weiUrnDesign.alpha ~ weiUrnDesign.beta

  private val queryUrnsFromId = for {
    id <- Parameters[Int]
    weiUrns <- WeiUrn if weiUrns.randomizationMethodId is id
  } yield weiUrns.id ~ weiUrns.treatmentArmId ~ weiUrns.stratum

  def create(randomizationMethod: WeiUrnDesignRandomization, trialId: Int): Validation[String, Int] = {
    database withSession {
      val identifier =
        threadLocalSession withTransaction {
          val seed = randomizationMethod.random.nextLong()
          randomizationMethod.random.setSeed(seed)
          RandomizationMethods.noId insert(trialId, generateBlob(randomizationMethod.random).get, randomizationMethod.getClass.getName, seed)
          val id = getId(trialId).toEither match {
            case Left(x) => return Failure(x)
            case Right(id1) => id1
          }
          WeiUrnDesignRandomizations.noId insert(0, Some(id), randomizationMethod.alpha, randomizationMethod.beta)

          id
        }
      Success(identifier)
    }
  }

  def get(id: Int): Validation[String, Option[WeiUrnDesignRandomization]] = {
    database withSession {
      val resultList = queryRandomizationMethodFromId(id).list
      if (resultList.isEmpty) Success(None)
      else if (resultList.size == 1) {
        val rm = resultList(0)
        if (rm._4 == classOf[WeiUrnDesignRandomization].getName) {
          val parameter = getWeiUrnParameter(id).toEither match {
            case Left(x) => return Failure(x)
            case Right(parameterRes) => parameterRes
          }
          val weiUrnDesignRandomization = new WeiUrnDesignRandomization(rm._1.get, 0, parameter._1, parameter._2)(deserializeRandomGenerator(rm._3))
          getUrns(weiUrnDesignRandomization)
          return Success(Some(weiUrnDesignRandomization))
        }  else {
          Failure("Wrong plugin")
        }
      } else Failure("Duplicated database entry")
    }
  }

  def getFromTrialId(trialId: Int): Validation[String, Option[WeiUrnDesignRandomization]] = {
    database withSession {
      val resultList = queryRandomizationMethodFromTrialId(trialId).list
      if (resultList.isEmpty) Success(None)
      else if (resultList.size == 1) {
        val rm = resultList(0)
        if (rm._4 == classOf[WeiUrnDesignRandomization].getName) {
          val parameter = getWeiUrnParameter(rm._1.get).toEither match {
            case Left(x) => return Failure(x)
            case Right(parameterRes) => parameterRes
          }
          val weiUrnDesignRandomization = new WeiUrnDesignRandomization(rm._1.get, 0, parameter._1, parameter._2)(deserializeRandomGenerator(rm._3))
          getUrns(weiUrnDesignRandomization)
          return Success(Some(weiUrnDesignRandomization))
        } else {
          Failure("Wrong plugin")
        }
      } else Failure("Duplicated database entry")
    }
  }


  def update(randomizationMethod: WeiUrnDesignRandomization): Validation[String, WeiUrnDesignRandomization] = {
    database withSession {
      threadLocalSession withTransaction {
        queryRandomizationMethodFromId(randomizationMethod.id).mutate {
          r =>
            r.row = r.row.copy(_3 = generateBlob(randomizationMethod.random).get, _4 = randomizationMethod.getClass.getName)
        }
        if (randomizationMethod.isInstanceOf[WeiUrnDesignRandomization]) {
          //update parameter
          queryWeiUrnDesignRandomizationFromId(randomizationMethod.id).mutate {
            r =>
              r.row = r.row.copy(_4 = randomizationMethod.alpha, _5 = randomizationMethod.beta)
          }
        }
        updateUrns(randomizationMethod)
      }
    }
    get(randomizationMethod.id).toEither match {
      case Left(x) => Failure(x)
      case Right(None) => Failure("Method not found")
      case Right(Some(weiUrnDesignRandomizationMethod)) => Success(weiUrnDesignRandomizationMethod)
    }
  }

  def delete(randomizationMethod: WeiUrnDesignRandomization) {
    database withSession {
      queryWeiUrnDesignRandomizationFromId(randomizationMethod.id).mutate {
        r =>
          r.delete()
      }

      queryRandomizationMethodFromId(randomizationMethod.id).mutate {
        r =>
          r.delete()
      }

    }

  }

  private def getWeiUrnParameter(id: Int): Validation[String, (Int, Int)] = {
    database withSession {
      val resultList = queryWeiUrnDesignRandomizationFromId(id).list
      if (resultList.isEmpty) Failure("Urn parameter not found")
      else if (resultList.size == 1) Success(resultList(0)._4, resultList(0)._5)
      else Failure("More than one urn parameters found")
    }
  }

  private def getUrns(weiUrnDesignRandomization: WeiUrnDesignRandomization) {
    database withSession {
      for (urn <- queryUrnsFromId(weiUrnDesignRandomization.id)) {
        if (weiUrnDesignRandomization.urns.get(urn._3).isEmpty) weiUrnDesignRandomization.urns.put(urn._3, new ListBuffer())
        weiUrnDesignRandomization.urns.get(urn._3).get.append(urn._2)
      }
    }
  }

  private def updateUrns(randomizationMethod: WeiUrnDesignRandomization) {
    deleteUrns(randomizationMethod)
    saveUrns(randomizationMethod)
  }

  private def deleteUrns(randomizationMethod: WeiUrnDesignRandomization) {
    queryUrnsFromId(randomizationMethod.id).mutate {
      urn =>
        urn.delete()
    }
  }

  private def saveUrns(randomizationMethod: WeiUrnDesignRandomization) {
    randomizationMethod.urns.foreach(entry => entry._2.foreach(armId => WeiUrn.noId insert(Some(randomizationMethod.id), armId, entry._1)))
  }

}

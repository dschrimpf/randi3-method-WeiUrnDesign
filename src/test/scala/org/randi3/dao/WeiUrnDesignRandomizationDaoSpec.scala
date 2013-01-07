package org.randi3.dao

import org.apache.commons.math3.random.MersenneTwister

import org.junit.runner.RunWith

import org.scalaquery.ql._
import org.scalaquery.session.Database.threadLocalSession


import org.scalatest.matchers.{ShouldMatchers, MustMatchers}
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import scala.Left
import org.randi3.randomization.WeiUrnDesignRandomization
import scala.Right
import scala.Some


@RunWith(classOf[JUnitRunner])
class WeiUrnDesignRandomizationDaoSpec extends FunSpec with MustMatchers with ShouldMatchers {

  import org.randi3.utility.TestingEnvironmentUrn._

  import driver.Implicit._

  import schemaUrn._

  describe("WeiUrnDesignRandomizationDao create method") {

    it("should be able to create a new block randomizationmethod with parameter (alpha and beta) and without urns") {
      val urnRandomization: WeiUrnDesignRandomization = new WeiUrnDesignRandomization(alpha = 5, beta = 3)(random = new MersenneTwister)
      val trialDB = createTrialDB
      val id = weiUrnDesignRandomizationDao.create(urnRandomization, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(idRes) => idRes
      }

      database withSession {
        val allBlockUrnRandomizations = Query(WeiUrnDesignRandomizations).list
        allBlockUrnRandomizations.size must be(1)
        allBlockUrnRandomizations.head._3 must be(Some(id))
        allBlockUrnRandomizations.head._4 must be(urnRandomization.alpha)
        allBlockUrnRandomizations.head._5 must be(urnRandomization.beta)
      }
    }


  }
}

package org.randi3.randomization

import org.junit.runner.RunWith
import org.scalatest.matchers.MustMatchers
import org.apache.commons.math3.random.MersenneTwister
import org.randi3.model._
import collection.mutable.ListBuffer
import org.randi3.model.criterion.{IntegerCriterion, OrdinalCriterion}
import org.randi3.model.criterion.constraint.{IntegerConstraint, OrdinalConstraint}
import collection.mutable
import scala.Some
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSpec

@RunWith(classOf[JUnitRunner])
class WeiUrnDesignRandomizationTest extends FunSpec with MustMatchers {

  import org.randi3.utility.TestingEnvironmentUrn._

  describe("Wei's urn design randomization method") {

    it("should be nearly balanced after the end of the trial") {

      val testParameters = List((8, 10))
      val runs = 1000

      for (testCase <- testParameters) {

        val runResults = new ListBuffer[(Int, Int)]()

        for (i <- 0 until runs) {

          val arms = new ListBuffer[TreatmentArm]()

          //create the arms
          arms.append(createTreatmentArm.copy(id = 1, plannedSize = 50))
          arms.append(createTreatmentArm.copy(id = 2, plannedSize = 50))

          val weiUrnDesignRandomizationMethod = new WeiUrnDesignRandomization(alpha = testCase._1, beta = testCase._2)(random = new MersenneTwister())
          val trial = createTrial.copy(treatmentArms = arms.toList, randomizationMethod = Some(weiUrnDesignRandomizationMethod))

          for (i <- 1 to trial.plannedSubjects) {
            val subject = TrialSubject(identifier = "subject" + i, investigatorUserName = "investigator", trialSite = trial.participatingSites.head, properties = Nil).toOption.get
            trial.randomize(subject).isSuccess must be(true)
            trial.getSubjects.size must be(i)
          }

          val sizeArm1 = trial.treatmentArms.find(arm => arm.id == 1).get.subjects.size
          val sizeArm2 = trial.treatmentArms.find(arm => arm.id == 2).get.subjects.size
          runResults.append((sizeArm1, sizeArm2))
        }

        runResults.size must be(runs)

        val meansTmp = runResults.toList.reduce((acc, act) => (acc._1 + act._1, acc._2 + act._2))
        val means = (meansTmp._1 / (runs * 1.0), meansTmp._2 / (runs * 1.0))

        means._1 must be >= (50 - 0.3)
        means._1 must be <= (50 + 0.3)

        means._2 must be >= (50 - 0.3)
        means._2 must be <= (50 + 0.3)

      }


    }

  }
}

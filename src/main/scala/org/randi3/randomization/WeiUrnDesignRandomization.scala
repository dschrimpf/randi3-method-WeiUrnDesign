package org.randi3.randomization

import org.randi3.model._
import org.apache.commons.math3.random._
import scala.collection.mutable.ListBuffer


import collection.mutable

case class WeiUrnDesignRandomization(id: Int = Int.MinValue, version: Int = 0, alpha: Int, beta: Int)(val random: RandomGenerator) extends RandomizationMethod {


  //Int because of easier database mapping
  val urns = new mutable.HashMap[String, ListBuffer[Int]]()


  def randomize(trial: Trial, subject: TrialSubject): TreatmentArm = {
    pullFromUrn(trial, subject)
  }

  private def pullFromUrn(trial: Trial, subject: TrialSubject): TreatmentArm = {
    val stratum = subject.getStratum(trial.stratifyTrialSite)
    val urn = urns.get(stratum).getOrElse {
      generateUrn(trial, subject)
      urns.get(stratum).get
    }

    val armPosition = random.nextInt(urn.size)
    val armId = urn(armPosition)
    urn.remove(armPosition)
    val otherArm = trial.treatmentArms.find(arm => arm.id != armId).get
    for (i <- 0 until beta) {
      urn.append(otherArm.id)
    }
    trial.treatmentArms.find(arm => arm.id == armId).get
  }

   private def generateUrn(trial: Trial, subject: TrialSubject) {
    val stratum = subject.getStratum(trial.stratifyTrialSite)
    val urn = urns.get(stratum).getOrElse {
      urns.put(stratum, new ListBuffer())
      urns.get(stratum).get
    }
    if (!urn.isEmpty) return

     for (arm <- trial.treatmentArms){
       for(i <- 0 until alpha) {
          urn.append(arm.id)
       }
     }
  }

}

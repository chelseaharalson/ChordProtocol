package project3

import akka.actor._

/**
 * Created by chelsea on 10/04/15.
 */

object Main {

  def main(args: Array[String]) = {
    if (!isNumber(args(0))) {
      println("Invalid number of nodes!")
      System.exit(0)
    }
    if (!isNumber(args(1))) {
      println("Invalid number of requests!")
      System.exit(0)
    }

    println("Number of Nodes: " + args(0) + " Number of Requests: " + args(1))

    val system = ActorSystem("Chord")
    val master = system.actorOf(Props(new MasterActor(args(0).toInt, args(1).toInt)), "master")
    master ! "CreateActors"
  }

  // Checks if parameter is a number
  def isNumber(inputString: String): Boolean = {
    inputString.forall(_.isDigit)
  }

}
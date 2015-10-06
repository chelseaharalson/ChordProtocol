package project3

import akka.actor._
import scala.util.Random
import scala.math._
import scala.collection.mutable.ArrayBuffer

/**
 * Created by chelsea on 10/04/15.
 */
class NodeActor(nodeID: String, predNode: String, succNode: String, numRequests: Int) extends Actor {

  var fingerTable = ArrayBuffer[String]()
  var hops = 0
  val m = 2

  println("NODE ACTOR    " + succNode)
  fingerTable.+=(succNode)

  def receive = {

    case msg: String => {
      if (msg.equals("InitFingerTable")) {
        println("INIT!!! " + nodeID)
        getFingerTable()
      }
    }

    case NodeFound(nodeID,hops) => {

    }

    case FindClosestPrecedingNode(startNodeID,fingerIdx) => {
      /*for (i <- 0 to fingerTable.size-1) {
        if (findNodeID < fingerTable(i)) {
          context.sender() ! FingerNodeFound(fingerTable(i), fingerIdx)
        }
      }*/

      val p = pow(2,fingerIdx)
      //println("TEST!!!!!" + nodeID + "   " + p + "   " + fingerTable(0))
      if (fingerTable(0) < p.toString) {
        context.actorSelection("../" + succNode) ! FindClosestPrecedingNode(startNodeID, fingerIdx)
      }
      else {
        println("FOUND!!!!!!! " + startNodeID + "    " + nodeID)
      }
    }

  }

  def locateNode(nodeID: String, startNode: String, hops: Int) = {
    if (fingerTable.size == 0) {
      context.actorSelection("../" + startNode) ! NodeFound(nodeID,1)
    }
  }

  def getFingerTable() = {
    //for (i <- 0 until m) {
      context.actorSelection("../" + succNode) ! FindClosestPrecedingNode(nodeID, 1)
    //}
  }

  def stabilize() = {

  }

  def join() = {

  }

  def sendMessage() = {

  }

}
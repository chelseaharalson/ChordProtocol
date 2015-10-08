package project3

import akka.actor._
import scala.util.Random
import scala.math._
import scala.collection.mutable.ArrayBuffer

/**
 * Created by chelsea on 10/04/15.
 */
class NodeActor(nodeID: String, predNode: String, succNode: String, numRequests: Int, numNodes: Int) extends Actor {

  var fingerTable = ArrayBuffer[String]()
  var hops = 0
  val m = 3

  fingerTable.+=(succNode)

  def receive = {

    case msg: String => {
      if (msg.equals("InitFingerTable")) {
        //println("INIT!!! " + nodeID)
        getFingerTable()
      }
    }

    case NodeFound(nodeID,hops) => {

    }

    case FindClosestPrecedingNode(startNodeID,fingerIdx) => {
      var nextNode = fingerTable(0)
      val p = getName((pow(2,fingerIdx) - 1).toInt)
      for (i <- 1 to fingerTable.size-1) {
        if (p >= fingerTable(i)) {
          nextNode = fingerTable(i)
        }
      }
      println("NEXT NODE: " + nextNode + "     p: " + p)
      if (p == nextNode.toString) {
        println("FOUND!!!! " + p)
      }
      else {
        context.actorSelection("../" + nextNode) ! FindClosestPrecedingNode(startNodeID,fingerIdx)
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
      //context.actorSelection("../" + succNode) ! FindClosestPrecedingNode(nodeID, 3)
    //}
    for (i <- 1 until m) {
      val n = (getID(nodeID) + pow(2,i)) % numNodes
      val nodeName = getName(n.toInt)
      fingerTable.+=(nodeName)
    }
    for (i <- 0 until m) {
      println("Node ID: " + nodeID + "   Finger Table: " + fingerTable(i))
    }
  }

  def stabilize() = {

  }

  def join() = {

  }

  def sendMessage() = {

  }

  def getName(idx: Int): String = {
    var result = ""
    if (idx < 10) {
      result = "000" + idx.toString
    }
    else if (idx < 100) {
      result = "00" + idx.toString
    }
    else if (idx < 1000) {
      result = "0" + idx.toString
    }
    else {
      result = idx.toString
    }
    result
  }

  def getID(nodeName: String): Int = {
    var result = 0
    result = nodeName.replaceFirst("^0+(?!$)", "").toInt
    result
  }

}
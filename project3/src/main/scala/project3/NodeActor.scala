package project3

import akka.actor._
import scala.util.Random
import scala.math._
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

/**
 * Created by chelsea on 10/04/15.
 */
class NodeActor(nodeID: String, predNode: String, succNode: String, numRequests: Int, numNodes: Int) extends Actor {

  var fingerTable = ArrayBuffer[String]()
  var hops = 0
  val m = 4

  fingerTable.+=(succNode)

  def receive = {

    case msg: String => {
      if (msg.equals("InitFingerTable")) {
        //println("INIT!!! " + nodeID)
        getFingerTable()
      }
    }

    case NodeFound(fnodeID,hops) => {
      println("FOUND!!!! " + fnodeID + "   HOPS: " + hops)
    }

    case SendMessages(numNodes,numRequests) => {
      val thread = new Thread {
        override def run {
          for (i <- 0 until numRequests) {
            val r = Random.nextInt(numNodes)
            Thread.sleep(1000)
            println("Random: " + getNodeName(r))
            context.actorSelection("../" + nodeID) ! LocateNode(getNodeName(r), nodeID, 0)
          }
        }
      }
      thread.start
    }

    case FindClosestPrecedingNode(startNodeID,fingerIdx) => {
      var nextNode = fingerTable(0)
      val p = getNodeName((pow(2,fingerIdx) - 1).toInt)
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

    case LocateNode(lnodeID,startNode,hops) => {
      var hops2 = hops
      var nextNode = ""
      var ft = ""
      //   for (i <- 0 until m) {
      //     println("Node ID: " + nodeID + "   Finger Table: " + fingerTable(i))
      //   }
      nextNode = fingerTable(0)
      breakable {
        for (i <- 0 until fingerTable.size) {
          ft = fingerTable(i)

          if (lnodeID == ft) {
            nextNode = fingerTable(i)
            break
          }
          else if (((lnodeID > startNode) && (lnodeID > ft) && (ft > startNode)) ||  // normal locate
                  ((lnodeID < startNode) && (lnodeID > ft)) ||  // locate crossing 0 - normal selection
                  ((lnodeID < startNode) && (ft > startNode))) { // locate crossing 0 - before 0 - left side always select
            nextNode = fingerTable(i)
            //println("ASSIGN NEXT NODE ID: " + nextNode)
          }
          else {
            break
          }
        }
      }
      hops2 += 1
      println("NEXT NODE: " + nextNode + "     p: " + lnodeID)
      if (lnodeID == nextNode) {
        //println("FOUND!!!! " + nodeID + "   HOPS: " + hops2)
        context.actorSelection("../" + startNode) ! NodeFound(lnodeID, hops2)
      }
      else {
        context.actorSelection("../" + nextNode) ! LocateNode(lnodeID, startNode, hops2)
      }
    }

  }


  def getFingerTable() = {
    //for (i <- 0 until m) {
      //context.actorSelection("../" + succNode) ! FindClosestPrecedingNode(nodeID, 3)
    //}
    for (i <- 1 until m) {
      val n = (getID(nodeID) + pow(2,i)) % numNodes
      val nodeName = getNodeName(n.toInt)
      fingerTable.+=(nodeName)
    }
    for (i <- 0 until m) {
      //println("Node ID: " + nodeID + "   Finger Table: " + fingerTable(i))
    }
  }

  def stabilize() = {

  }

  def join() = {

  }

  def sendMessage() = {

  }

  def getNodeName(idx: Int): String = {
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
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
  //val m = sqrt(numNodes).toInt
  val m = 7
  var finishedCount = 0
  var hopsCount = 0

  fingerTable.+=(succNode)

  def receive = {

    case msg: String => {
      if (msg.equals("InitFingerTable")) {
        //println("INIT!!! " + nodeID)
        getFingerTable()
      }
      if (msg.equals("PrintFingerTable")) {
        for (i <- 0 until fingerTable.size) {
          println(fingerTable(i) + "   " + (80 + pow(2,i)))
        }
      }
    }

    case NodeFinished(fnodeID,hops,mID) => {
      finishedCount += 1
      hopsCount += hops
      if (mID >= 0) fingerTable(mID) = fnodeID
      if (finishedCount == numRequests) {
        //println("FINISHED!!!! " + fnodeID + "   HOPS: " + hopsCount)
        context.parent ! NodeFinished(nodeID,hopsCount,mID)
      }
    }

    case SendMessages(numNodes,numRequests) => {
      val thread = new Thread {
        override def run {
          for (i <- 0 until numRequests) {
            val r = Random.nextInt(numNodes)
            Thread.sleep(1000)
            //println("Random: " + getNodeName(r))
            context.actorSelection("../" + nodeID) ! LocateNode(getNodeName(r), nodeID, 0, -1)
          }
        }
      }
      thread.start
    }

    case Stabilize(pNodeID) => {
      val iNodeID = getID(pNodeID)
      fingerTable.clear()
      for (i <- 0 until m) {
        fingerTable.+=("")
        val l = iNodeID + pow(2,i)
        //println("GET NODE NAME: " + getNodeName(l.toInt))
        context.actorSelection("../" + pNodeID) ! LocateNode(getNodeName(l.toInt), pNodeID, 0, i)
      }
    }

    case LocateNode(lnodeID,startNode,hops,mID) => {
      var hops2 = hops
      var nextNode = ""
      var ft = ""
      var found = false

      nextNode = fingerTable(0)
      breakable {
        for (i <- 0 until fingerTable.size) {
          if (fingerTable(i) != "") {
            ft = fingerTable(i)

            if (lnodeID == ft) {
              nextNode = fingerTable(i)
              found = true
              break
            }
            else if (((lnodeID > startNode) && (lnodeID > ft) && (ft > startNode)) || // normal locate
              ((lnodeID < startNode) && (lnodeID > ft)) || // locate crossing 0 - normal selection
              ((lnodeID < startNode) && (ft > startNode))) {
              // locate crossing 0 - before 0 - left side always select
              nextNode = fingerTable(i)
              found = true
              //println("ASSIGN NEXT NODE ID: " + nextNode)
            }
            else {
              break
            }
          }
        }
      }

      if ( (getID(lnodeID) % numNodes >= getID(succNode)) && (!found) ) {
        found = true
        nextNode = succNode
      }



      hops2 += 1
      //println("NEXT NODE: " + nextNode + "     p: " + lnodeID)
      if (!found) context.actorSelection("../" + startNode) ! ClosestNode(succNode,mID)
      else if (lnodeID == nextNode) context.actorSelection("../" + startNode) ! NodeFinished(lnodeID, hops2,mID)
      else context.actorSelection("../" + nextNode) ! LocateNode(lnodeID, startNode, hops2, mID)
    }

    case ClosestNode(pnodeID,mID) => {
      //println("Closest Node: " + pnodeID + "   MID: " + mID + "   pow: " + pow(2,mID))
      if (mID >= 0) fingerTable(mID) = pnodeID
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
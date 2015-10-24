package project3

import akka.actor._
import scala.util.Random
import scala.math._
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

/**
 * Created by chelsea on 10/04/15.
 */
class NodeActor(nodeID: String, pPredNode: String, pSuccNode: String, numRequests: Int, numNodes: Int) extends Actor {

  var fingerTable = ArrayBuffer[String]()
  var hops = 0
  //val m = sqrt(numNodes).toInt
  //val m = 7
  val m = 1
  //val m = 4
  //val m = 2
  var finishedCount = 0
  var hopsCount = 0
  val locateMode = "LocateMode"
  val joinMode = "JoinMode"
  var mode = locateMode
  var predNode = pPredNode
  var succNode = pSuccNode

  def receive = {

    case msg: String => {
      if (msg.equals("InitFingerTable")) {
        //println("INIT!!! " + nodeID)
        getFingerTable()
      }
      if (msg.equals("PrintFingerTable")) {
        for (i <- 0 until fingerTable.size) {
          println(fingerTable(i) + "   " + (getID(nodeID) + pow(2,i)))
        }
      }
      if (msg.equals("Predecessor")) {
        println("Node ID: " + nodeID + "  Pred Node " + predNode)
      }
      if (msg.equals("Successor")) {
        println("Node ID: " + nodeID + "  Succ Node " + succNode)
      }
    }

    // Received when there is an exact match on the node
    case FoundNode(fnodeID,hops,mID) => {
      finishedCount += 1
      hopsCount += hops
      if (mID >= 0) fingerTable(mID) = fnodeID
      if (finishedCount == numRequests) {
        //println("FINISHED!!!! " + fnodeID + "   HOPS: " + hopsCount)
        context.parent ! FoundNode(nodeID,hopsCount,mID)
      }
      //println("FINISHED!!!! " + fnodeID + "   HOPS: " + hopsCount)
    }

    case ClosestNode(pnodeID,mID) => {
      //println("Closest Node: " + pnodeID + "   MID: " + mID + "   pow: " + pow(2,mID))
      if (mID >= 0) fingerTable(mID) = pnodeID
    }

    // Chooses random number and sends message to that node until the number of requests
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

    // Sync all the nodes. Clears all the fingers and then rebuilds the finger table.
    case Stabilize(pNodeID) => {
      stabilizeNode(pNodeID)
    }

    case StabilizeAllNodes(pNodeID) => {
      if (succNode != pNodeID) {
        println("PNODE ID: " + pNodeID + " SUCC NODE: " + succNode)
        stabilizeNode(nodeID)
        context.actorSelection("../" + succNode) ! StabilizeAllNodes(pNodeID)
      }
    }

    // Find closest or exact match
    case LocateNode(lnodeID,startNode,hops,mID) => {
      // if mID == -2, joinMode
      // if mID == -1, locateMode
      // if mID >= 0, find fingerTable
      var hops2 = hops
      var nextNode = ""
      var ft = ""
      var found = false

      nextNode = succNode
      //if (mID > -1) println("lnodeID:   " + lnodeID + "   NODE ID: " + nodeID + "   START NODE: " + startNode)
      breakable {
        for (i <- 0 until fingerTable.size) {
          if (fingerTable(i) != "") {
            ft = fingerTable(i)

            if (lnodeID == ft) {
              nextNode = fingerTable(i)
              found = true
              //println("FOUND!!!! " + nextNode)
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

      //println("LNODE ID: " + lnodeID + " SUCC NODE: " + succNode + " START NODE: " + startNode)
      if (
        ((getID(lnodeID) >= getID(succNode))
            ||
          ( (getID(lnodeID) <= getID(succNode) ) && ( (getID(lnodeID) <= getID(startNode)))
          ))
           && (!found)) {
        //if (mID > -1) println("FOUND: " + found + "  NEXT NODE: " + nextNode)
      //if ( (getID(lnodeID) > getID(succNode)) && (!found) ) {
        found = true
        nextNode = succNode
        //println("CHELSEA NEXT NODE: " + nextNode + "     p: " + lnodeID + "   Found: " + found)
      }

      hops2 += 1
      //if (mID == -2) println("NEXT NODE: " + nextNode + "     l: " + lnodeID + "   Found: " + found)
      if (!found) {
        //println("lnode: " + lnodeID + "  CLOSEST   " + succNode)
        if (mID == -2) {
          // join mode
          context.actorSelection("../" + succNode) ! UpdateNodes(lnodeID)
        }
        else {
          println("!!!!!!!!!!!@@@@@@@@@@@@@######### " + lnodeID + "    " + succNode + "    " + nodeID)
          context.actorSelection("../" + startNode) ! ClosestNode(succNode, mID)
        }
      }
      else if (getID(lnodeID) % (numNodes * 2) == getID(nextNode)) {
        //println("lnode: " + lnodeID + "  FINISHED   " + nextNode + "   MOD: " + getID(lnodeID) % numNodes)
        if (mID == -2) {
          // join mode
          context.actorSelection("../" + succNode) ! UpdateNodes(lnodeID)
        }
        else {
          context.actorSelection("../" + startNode) ! FoundNode(nextNode, hops2, mID)
        }
      }
      else {
        //println("lnode: " + lnodeID + "  LOCATE   " + nextNode)
        context.actorSelection("../" + nextNode) ! LocateNode(lnodeID, startNode, hops2, mID)
      }
    }

    case UpdateNodes(newNodeID) => {
      context.actorSelection("../" + predNode) ! UpdateSuccNode(newNodeID)
      context.actorSelection("../" + newNodeID) ! UpdatePredNode(predNode)
      context.actorSelection("../" + newNodeID) ! UpdateSuccNode(nodeID)
      predNode = newNodeID
    }

    case Join(startNodeID,newNodeID) => {
      context.actorSelection("../" + startNodeID) ! LocateNode(newNodeID, startNodeID, 0, -2)
    }

    case UpdatePredNode(newNodeID) => {
      //if (newNodeID != nodeID) {
        //context.actorSelection("../" + predNode) ! UpdateSuccNode(newNodeID)
        //context.actorSelection("../" + newNodeID) ! UpdatePredNode(nodeID)
      //}
      predNode = newNodeID
    }

    case UpdateSuccNode(newNodeID) => {
      succNode = newNodeID
      //context.actorSelection("../" + newNodeID) ! UpdatePredNode(nodeID)
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

  def stabilizeNode(pNodeID: String) = {
    val iNodeID = getID(pNodeID)
    fingerTable.clear()
    for (i <- 0 until m) {
      fingerTable.+=("")
      val l = (iNodeID + pow(2,i)) % (numNodes * 2)
      println("PNODE ID: " + pNodeID + "   GET NODE NAME: " + getNodeName(l.toInt))
      context.actorSelection("../" + pNodeID) ! LocateNode(getNodeName(l.toInt), pNodeID, 0, i)
    }
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
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
  //val m = 1
  val m = 4
  //val m = 2
  var finishedCount = 0
  var hopsCount = 0
  var predNode = pPredNode
  var succNode = pSuccNode
  var sent = false

  def receive = {

    case msg: String => {
      if (msg.equals("PrintFingerTable")) {
        for (i <- 0 until fingerTable.size) {
          println((getID(nodeID) + pow(2,i)) + "    " + fingerTable(i))
        }
      }
      if (msg.equals("Predecessor")) {
        println("Node ID: " + nodeID + "  Pred Node " + predNode)
      }
      if (msg.equals("Successor")) {
        println("Node ID: " + nodeID + "  Succ Node " + succNode)
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
      // Tries to locate node through finger table.
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
              ((lnodeID < startNode) && (ft > startNode))) { // locate crossing 0 - before 0 - left side always select
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

      // Tries to locate node through successor node.
      if (
        ( (lnodeID > startNode) && (lnodeID > succNode) && (succNode > startNode) ) // not crossing 0
        || ( (lnodeID < startNode) && (lnodeID > succNode) ) // crosses 0
        || ( (lnodeID < startNode) && (succNode > startNode) ) // locate crossing 0 - before 0 - left side always select
        && (!found) ) {
          found = true
          nextNode = succNode
        }

      hops2 += 1
      if (!found) {
        //println("lnode: " + lnodeID + "  CLOSEST   " + succNode)
        if (mID == -2) {
          // join mode
          // Succ Node is the closest largest node, so succ node gets the message to update nodes
          context.actorSelection("../" + succNode) ! UpdateNodes(lnodeID)
        }
        else {
          //println("!!!!!!!!!!!@@@@@@@@@@@@@######### " + lnodeID + "    " + succNode + "    " + nodeID)
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

    // Received when there is an exact match on the node
    case FoundNode(fnodeID,hops,mID) => {
      // Fill in finger table
      if (mID >= 0) fingerTable(mID) = fnodeID
      // Send messages
      if (mID == -3) {
        finishedCount += 1
        hopsCount += hops
      }
      if (finishedCount == numRequests) {
        //println("FINISHED!!!! " + fnodeID + "   HOPS: " + hopsCount)
        context.parent ! FinishedMessage(nodeID,hopsCount)
      }
    }

    // Not an exact match on the closest node, but found next largest one.
    case ClosestNode(pnodeID,mID) => {
      if (mID >= 0) fingerTable(mID) = pnodeID
      /*println("Closest Node: " + pnodeID + "   MID: " + mID + "   pow: " + pow(2,mID) + "  Finger table MID: "
        + fingerTable(mID) + "   Node ID: " + nodeID)*/
    }

    // Stabilize 1 node
    case Stabilize(pNodeID) => {
      stabilizeNode(pNodeID)
    }

    // Goes from successor to successor and sends a message to stabilize. pNodeID is starting point.
    case StabilizeAllNodes(pNodeID) => {
      if (succNode != pNodeID) {
        //println("PNODE ID: " + pNodeID + " SUCC NODE: " + succNode)
        stabilizeNode(nodeID)
        context.actorSelection("../" + succNode) ! StabilizeAllNodes(pNodeID)
      }
    }

    // Join function
    case Join(startNodeID,newNodeID) => {
      // Finds the next largest node to the desired node to be inserted
      // Needs to know 1 node to start searching from
      context.actorSelection("../" + startNodeID) ! LocateNode(newNodeID, startNodeID, 0, -2)
    }

    // Update nodes with new predecessor and successor
    case UpdateNodes(newNodeID) => {
      context.actorSelection("../" + predNode) ! UpdateSuccNode(newNodeID)
      context.actorSelection("../" + newNodeID) ! UpdatePredNode(predNode)
      context.actorSelection("../" + newNodeID) ! UpdateSuccNode(nodeID)
      predNode = newNodeID
    }

    case UpdatePredNode(newNodeID) => {
      predNode = newNodeID
    }

    case UpdateSuccNode(newNodeID) => {
      succNode = newNodeID
    }

    // Chooses random number and sends message to that node until the number of requests
    case SendMessages(pNodeID,numNodes,numRequests) => {
      // Goes from successor to successor until it reaches itself.
      // One node gets the send message request and sends it around to all other nodes.
      if (succNode != pNodeID) {
        context.actorSelection("../" + succNode) ! SendMessages(pNodeID,numNodes,numRequests)
      }
      // Node that is sending messages creates a new thread so the node can continuously send messages
      val thread = new Thread {
        override def run {
          for (i <- 0 until numRequests) {
            var r = getID(nodeID)
            while (r == getID(nodeID) || r == 0) {
              r = Random.nextInt(numNodes) * 2
            }
            Thread.sleep(1000)
            //println("Random: " + getNodeName(r))
            context.actorSelection("../" + nodeID) ! LocateNode(getNodeName(r), nodeID, 0, -3)
          }
        }
      }
      thread.start
    }

  }

  // Fills in finger table. Clears all the node IDs and go from 0 to m.
  def stabilizeNode(pNodeID: String) = {
    val iNodeID = getID(pNodeID)
    fingerTable.clear()
    for (i <- 0 until m) {
      fingerTable.+=("")
      var l = (iNodeID + pow(2,i)) % (numNodes * 2)
      // Handles the last node
      if (iNodeID > 0 && l == 0) l = iNodeID
      // Try to locate that node
      context.actorSelection("../" + pNodeID) ! LocateNode(getNodeName(l.toInt), pNodeID, 0, i)
    }
  }

  // Adds the zeros for the node name
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

  // Get the integer value of the name
  def getID(nodeName: String): Int = {
    var result = 0
    result = nodeName.replaceFirst("^0+(?!$)", "").toInt
    result
  }

}
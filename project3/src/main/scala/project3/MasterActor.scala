package project3

import akka.actor._
import scala.math._
import scala.util.Random

/**
 * Created by chelsea on 10/04/15.
 */
class MasterActor(numNodes: Int, numRequests: Int) extends Actor {

  var finishedCount = 0
  var hopsCount = 0
  //var numNodes = numOfNodes * 2

  def receive = {

    case msg: String => {
      if (msg.equals("CreateActors")) {
        createRingOne()
        //createRingTwo()
        //context.actorSelection(getNodeName(0)) ! StabilizeAllNodes(getNodeName(0))
        Thread.sleep(5000)
        println("================")
        context.actorSelection(getNodeName(2 * numNodes)) ! Stabilize(getNodeName(2 * numNodes))
        //Thread.sleep(5000)

        //context.actorSelection(getNodeName(0)) ! "Predecessor"
        //Thread.sleep(200)
        //context.actorSelection(getNodeName(0)) ! "Successor"
        context.actorSelection(getNodeName(2 * numNodes)) ! "PrintFingerTable"
        //context.actorSelection(getNodeName(20)) ! LocateNode(getNodeName(0), getNodeName(20), 0, -1)

        //context.actorSelection(getNodeName(16)) ! Join(getNodeName(16), getNodeName(20))
        //insertNode(getNodeName(20),getNodeName(16))
        /*Thread.sleep(200)
        context.actorSelection(getNodeName(16)) ! "Successor"
        Thread.sleep(200)
        context.actorSelection(getNodeName(16)) ! "Predecessor"
        Thread.sleep(200)


        context.actorSelection(getNodeName(20)) ! "Successor"
        Thread.sleep(200)
        context.actorSelection(getNodeName(20)) ! "Predecessor"
        Thread.sleep(200)


        context.actorSelection(getNodeName(32)) ! "Successor"
        Thread.sleep(200)
        context.actorSelection(getNodeName(32)) ! "Predecessor"
        Thread.sleep(200)*/


        // TEST
        //context.actorSelection(getNodeName(80)) ! LocateNode("0144","0080",0,-1)
        //context.actorSelection(getNodeName(14)) ! LocateNode("0004","0014",0,-1)
        //context.actorSelection(getNodeName(0)) ! LocateNode("0009","0000",0,-1)
        //context.actorSelection(getNodeName(4)) ! LocateNode("0002","0004",0,-1)
        /*context.actorSelection(getNodeName(8)) ! Stabilize("0008")
        Thread.sleep(5000)
        context.actorSelection(getNodeName(8)) ! "PrintFingerTable"*/

      }
    }

    case FoundNode(fnodeID,hops,mID) => {
      finishedCount += 1
      hopsCount += hops
      println("Finished Count: " + finishedCount + "  Node ID: " + fnodeID + "  Average Hops: " + hops/numRequests)
      if (finishedCount == numNodes) {
        println("Total Average Hops: " + hopsCount/(numRequests * numNodes))
      }
    }

  }

  def insertNode(newNodeID: String, baseNodeID: String) = {
    context.actorOf(Props(new NodeActor(newNodeID, getNodeName(0), getNodeName(0), numRequests, numNodes)), newNodeID)
    Thread.sleep(200)
    context.actorSelection(baseNodeID) ! Join(baseNodeID, newNodeID)
    Thread.sleep(200)
    //context.actorSelection(newNodeID) ! Stabilize(newNodeID)
    //Thread.sleep(5000)
    //context.actorSelection(newNodeID) ! "PrintFingerTable"
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

  def createRingOne() = {
    context.actorOf(Props(new NodeActor(getNodeName(0), getNodeName(numNodes*2), getNodeName(numNodes*2), numRequests, numNodes)), getNodeName(0))
    Thread.sleep(200)
    context.actorOf(Props(new NodeActor(getNodeName(numNodes*2), getNodeName(0), getNodeName(0), numRequests, numNodes)), getNodeName(numNodes*2))
    Thread.sleep(200)
    for (i <- 1 until numNodes) {
      //println(i*2)
      insertNode(getNodeName(i*2),getNodeName(0))
    }
  }

  def createRingTwo() = {
    // Small set
    context.actorOf(Props(new NodeActor(getNodeName(16), getNodeName(112), getNodeName(32), numRequests, numNodes)), getNodeName(16))
    Thread.sleep(200)

    context.actorOf(Props(new NodeActor(getNodeName(32), getNodeName(16), getNodeName(45), numRequests, numNodes)), getNodeName(32))
    Thread.sleep(200)

    context.actorOf(Props(new NodeActor(getNodeName(45), getNodeName(32), getNodeName(80), numRequests, numNodes)), getNodeName(45))
    Thread.sleep(200)

    context.actorOf(Props(new NodeActor(getNodeName(80), getNodeName(45), getNodeName(96), numRequests, numNodes)), getNodeName(80))
    Thread.sleep(200)

    context.actorOf(Props(new NodeActor(getNodeName(96), getNodeName(80), getNodeName(112), numRequests, numNodes)), getNodeName(96))
    Thread.sleep(200)

    context.actorOf(Props(new NodeActor(getNodeName(112), getNodeName(96), getNodeName(16), numRequests, numNodes)), getNodeName(112))
    Thread.sleep(200)
  }

}
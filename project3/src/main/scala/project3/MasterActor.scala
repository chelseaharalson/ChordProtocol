package project3

import akka.actor._

/**
 * Created by chelsea on 10/04/15.
 */
class MasterActor(numNodes: Int, numRequests: Int) extends Actor {

  var finishedCount = 0
  var hopsCount = 0

  def receive = {

    case msg: String => {
      if (msg.equals("CreateActors")) {
        createRingOne()
        //createRingTwo()
        context.actorSelection(getNodeName(2)) ! StabilizeAllNodes(getNodeName(2))
        context.actorSelection(getNodeName(numNodes * 2)) ! Stabilize(getNodeName(numNodes * 2))
        insertNode(getNodeName(3),getNodeName(2),true)
        Thread.sleep(5000)
        println("================")
        context.actorSelection(getNodeName(2)) ! SendMessages(getNodeName(2),numNodes, numRequests)
        context.actorSelection(getNodeName(3)) ! "PrintFingerTable"
      }
    }

    case FinishedMessage(fnodeID,hops) => {
      finishedCount += 1
      hopsCount += hops
      println("Finished Count: " + finishedCount + "  Node ID: " + fnodeID + "  Average Hops: " + hops/numRequests)
      if (finishedCount == numNodes) {
        println("Total Average Hops: " + hopsCount/(numRequests * numNodes))
        System.exit(0)
      }
    }

  }

  // Inserts node into the finger table
  def insertNode(newNodeID: String, baseNodeID: String, stabilize: Boolean) = {
    context.actorOf(Props(new NodeActor(newNodeID, getNodeName(0), getNodeName(0), numRequests, numNodes)), newNodeID)
    Thread.sleep(200)
    context.actorSelection(baseNodeID) ! Join(baseNodeID, newNodeID)
    Thread.sleep(200)
    if (stabilize) {
      context.actorSelection(newNodeID) ! Stabilize(newNodeID)
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

  def createRingOne() = {
    context.actorOf(Props(new NodeActor(getNodeName(2), getNodeName(numNodes*2), getNodeName(numNodes*2), numRequests, numNodes)), getNodeName(2))
    Thread.sleep(200)
    context.actorOf(Props(new NodeActor(getNodeName(numNodes*2), getNodeName(2), getNodeName(2), numRequests, numNodes)), getNodeName(numNodes*2))
    Thread.sleep(200)
    for (i <- 2 until numNodes) {
      //println(i*2)
      insertNode(getNodeName(i*2),getNodeName(2),false)
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
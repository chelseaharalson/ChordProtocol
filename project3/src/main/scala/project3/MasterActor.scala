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

  def receive = {

    case msg: String => {
      if (msg.equals("CreateActors")) {
        var predNode = 0
        var succNode = 0
        /*for (i <- 0 until numNodes) {
          // Create the actors
          if (i == 0) {
            predNode = numNodes-1
            succNode = i + 1
          }
          else if (i == numNodes-1) {
            predNode = i - 1
            succNode = 0
          }
          else {
            predNode = i - 1
            succNode = i + 1
          }

          //println(predNode + "   " + succNode)
          context.actorOf(Props(new NodeActor(getNodeName(i), getNodeName(predNode), getNodeName(succNode), numRequests, numNodes)), getNodeName(i))
          Thread.sleep(200)
        }
        for (i <- 0 until numNodes) {
          context.actorSelection(getNodeName(i)) ! "InitFingerTable"
        }
   //     context.actorSelection(getNodeName(14)) ! LocateNode("0004","0014",0)
        //context.actorSelection(getNodeName(0)) ! LocateNode("0009","0000",0)
   //     context.actorSelection(getNodeName(4)) ! LocateNode("0002","0004",0)
   //     context.actorSelection(getNodeName(14)) ! LocateNode("0004","0014",0)

        for (i <- 0 until numNodes) {
          context.actorSelection(getNodeName(i)) ! SendMessages(numNodes, numRequests)
        }*/


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

        //context.actorSelection(getNodeName(80)) ! LocateNode("0081","0080",0,-1)

        context.actorSelection(getNodeName(80)) ! Stabilize("0080")
        Thread.sleep(5000)
        context.actorSelection(getNodeName(80)) ! "PrintFingerTable"

      }
    }

    case NodeFinished(fnodeID,hops,mID) => {
      finishedCount += 1
      hopsCount += hops
      println("Finished Count: " + finishedCount + "  Node ID: " + fnodeID + "  Average Hops: " + hops/numRequests)
      if (finishedCount == numNodes) {
        println("Total Average Hops: " + hopsCount/(numRequests * numNodes))
      }
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

}
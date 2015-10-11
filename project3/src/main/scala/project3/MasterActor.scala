package project3

import akka.actor._
import scala.math._
import scala.util.Random

/**
 * Created by chelsea on 10/04/15.
 */
class MasterActor(numNodes: Int, numRequests: Int) extends Actor {

  def receive = {

    case msg: String => {
      if (msg.equals("CreateActors")) {
        var predNode = 0
        var succNode = 0
        for (i <- 0 until numNodes) {
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
    //    context.actorSelection(getNodeName(14)) ! LocateNode("0004","0014",0)
     //   context.actorSelection(getNodeName(0)) ! LocateNode("0009","0000",0)
        context.actorSelection(getNodeName(4)) ! LocateNode("0002","0004",0)
   //     context.actorSelection(getNodeName(14)) ! LocateNode("0004","0014",0)

        //context.actorSelection(getNodeName(0)) ! SendMessages(numNodes,numRequests)
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
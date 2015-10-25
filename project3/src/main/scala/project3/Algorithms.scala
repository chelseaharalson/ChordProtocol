package project3

/**
 * Created by chelsea on 10/04/15.
 */

case class Stabilize(pNodeID: String)
case class LocateNode(lnodeID: String, startNode: String, hops: Int, mID: Int)
case class SendMessages(pNodeID: String, numNodes: Int, numRequests: Int)
case class FoundNode(fnodeID: String, hops: Int, mID: Int)
case class FinishedMessage(fnodeID: String, hops: Int)
case class ClosestNode(pnodeID: String, mID: Int)
case class Join(startNodeID: String, newNodeID: String)
case class UpdateSuccNode(newNodeID: String)
case class UpdatePredNode(newNodeID: String)
case class UpdateNodes(newNodeID: String)
case class StabilizeAllNodes(pNodeID: String)
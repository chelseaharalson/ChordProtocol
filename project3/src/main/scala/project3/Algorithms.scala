package project3

/**
 * Created by chelsea on 10/04/15.
 */

case class FindClosestPrecedingNode(startNodeID: String, fingerIdx: Int)
case class FingerNodeFound(foundNodeID: String, fingerIdx: String)
case class LocateNode(lnodeID: String, startNode: String, hops: Int)
case class SendMessages(numNodes: Int, numRequests: Int)
case class NodeFinished(fnodeID: String, hops: Int)
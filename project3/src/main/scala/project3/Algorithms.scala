package project3

/**
 * Created by chelsea on 10/04/15.
 */

case class NodeFound(nodeID: String, hops: Int)
case class FindClosestPrecedingNode(startNodeID: String, fingerIdx: Int)
case class FingerNodeFound(foundNodeID: String, fingerIdx: String)
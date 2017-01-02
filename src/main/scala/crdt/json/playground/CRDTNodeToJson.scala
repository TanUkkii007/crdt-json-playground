package crdt.json.playground

import eu.timepit.crjdt.core.Node.{ListNode, MapNode, RegNode}
import eu.timepit.crjdt.core._
import io.circe.Json
import scala.annotation.tailrec
import cats.syntax.order._

trait CRDTNodeToJson {

  def mapToJson(mapNode: MapNode)(implicit rcr: RegisterConflictResolver): Json = {
    if (mapNode.entries.contains(TypeTag.MapT(Key.DocK))) {
      mapNode.entries.head match {
        case (TypeTag.MapT(Key.DocK), node: MapNode) => mapToJson(node)
      }
    } else {
      val fields = mapNode.entries.collect {
        case (TypeTag.MapT(Key.StrK(key)), node: MapNode) if mapNode.getPres(Key.StrK(key)).nonEmpty =>
          key -> mapToJson(node)
        case (TypeTag.ListT(Key.StrK(key)), node: ListNode) if mapNode.getPres(Key.StrK(key)).nonEmpty =>
          key -> listToJson(node)
        case (TypeTag.RegT(Key.StrK(key)), node: RegNode) if mapNode.getPres(Key.StrK(key)).nonEmpty =>
          key -> rcr.registerToJson(node)
      }
      Json.fromFields(fields)
    }
  }

  def listToJson(listNode: ListNode)(implicit rcr: RegisterConflictResolver): Json = {
    @tailrec
    def loopOrder(listRef: ListRef, keyOrder: Vector[Key]): Vector[Key] = {
      listRef match {
        case keyRef: KeyRef =>
          val key = keyRef.toKey
          val next = listNode.order(keyRef)
          if (listNode.getPres(key).nonEmpty) {
            loopOrder(next, keyOrder :+ key)
          } else {
            loopOrder(next, keyOrder)
          }
        case ListRef.TailR => keyOrder
      }
    }
    val keyOrder = loopOrder(ListRef.HeadR, Vector.empty).zipWithIndex.toMap
    val jsons = new Array[Json](keyOrder.size)
    listNode.entries.foreach {
      case (TypeTag.MapT(key), node: MapNode) if listNode.getPres(key).nonEmpty =>
        jsons(keyOrder(key)) = mapToJson(node)
      case (TypeTag.ListT(key), node: ListNode) if listNode.getPres(key).nonEmpty =>
        jsons(keyOrder(key)) = listToJson(node)
      case (TypeTag.RegT(key), node: RegNode) if listNode.getPres(key).nonEmpty =>
        jsons(keyOrder(key)) = rcr.registerToJson(node)
    }
    Json.fromValues(jsons)
  }

}

trait RegisterConflictResolver {

  def registerToJson(regNode: RegNode): Json

  protected def valToJson(value: Val): Json = value match {
    case Val.False => Json.False
    case Val.True => Json.True
    case Val.Null => Json.Null
    case Val.Num(n) => Json.fromBigDecimal(n)
    case Val.Str(s) => Json.fromString(s)
  }
}

object RegisterConflictResolver {

  implicit val LWW = new RegisterConflictResolver {
    override def registerToJson(regNode: RegNode): Json = {
      val lastVal = regNode.values.max(new Ordering[(Id, Val)] {
        override def compare(x: (Id, Val), y: (Id, Val)): Int = x._1 compare y._1
      })._2
      valToJson(lastVal)
    }
  }

  implicit val ListAll = new RegisterConflictResolver {
    override def registerToJson(regNode: RegNode): Json = {
      val items = regNode.values.map {
        case (id, value) => valToJson(value)
      }
      Json.arr(items.toSeq: _*)
    }
  }
}

object CRDTNodeToJson extends CRDTNodeToJson {
  implicit class PinpNode(node: Node)(implicit rcr: RegisterConflictResolver) {
    def toJson: Json = {
      node match {
        case v: MapNode => mapToJson(v)
        case v: ListNode => listToJson(v)
        case v: RegNode => rcr.registerToJson(v)
      }
    }
  }
}
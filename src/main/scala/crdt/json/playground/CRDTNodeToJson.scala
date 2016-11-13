package crdt.json.playground

import eu.timepit.crjdt.core.Node.{ListNode, MapNode, RegNode}
import eu.timepit.crjdt.core._
import io.circe.Json
import io.circe.Json._
import scala.collection.SortedMap
import scala.collection.immutable.Iterable

trait CRDTNodeToJson {

  def mapToJson(mapNode: MapNode): Json = {
    if (mapNode.entries.contains(TypeTag.MapT(Key.DocK))) {
      mapNode.entries.head match {
        case (TypeTag.MapT(Key.DocK), node: MapNode) => mapToJson(node)
      }
    } else {
      val fields = mapNode.entries.collect {
        case (TypeTag.MapT(Key.StrK(key)), node: MapNode) => key -> mapToJson(node)
        case (TypeTag.ListT(Key.StrK(key)), node: ListNode) => key -> listToJson(node)
        case (TypeTag.RegT(Key.StrK(key)), node: RegNode) if node.values.nonEmpty => key -> registerToJson(node)
      }
      Json.fromFields(fields)
    }
  }

  def listToJson(listNode: ListNode): Json = {
    val jsons = listNode.entries.map {
      case (TypeTag.MapT(key), node: MapNode) => mapToJson(node)
      case (TypeTag.ListT(key), node: ListNode) => listToJson(node)
      case (TypeTag.RegT(key), node: RegNode) => registerToJson(node)
    }
    Json.fromValues(jsons)
//    SortedMap(listNode.order.toSeq: _*)(new Ordering[ListRef] {
//      override def compare(x: ListRef, y: ListRef): Int = {
//        case (ListRef.IdR(id1), ListRef.IdR(id2)) => Id.orderingId.compare(id1, id2)
//        case (ListRef.HeadR, _) =>
//      }
//    })
  }

  def registerToJson(regNode: RegNode): Json = {
    val items = regNode.values.map {
      case (id, value) => value match {
        case Val.False => Json.False
        case Val.True => Json.True
        case Val.Null => Json.Null
        case Val.Num(n) => Json.fromBigDecimal(n)
        case Val.Str(s) => Json.fromString(s)
      }
    }
    items match {
      case Nil => Json.Null
      case head :: Nil => head
      case list => Json.fromString(list.map(_.toString()).mkString(" / "))
    }
  }
}

object CRDTNodeToJson extends CRDTNodeToJson {
  implicit class PinpNode(node: Node) {
    def toJson: Json = {
      node match {
        case v: MapNode => mapToJson(v)
        case v: ListNode => listToJson(v)
        case v: RegNode => registerToJson(v)
      }
    }
  }
}
package crdt.json.playground

import eu.timepit.crjdt.core.Node.{ListNode, MapNode, RegNode}
import eu.timepit.crjdt.core._
import io.circe.Json
import scala.annotation.tailrec

trait CRDTNodeToJson {

  def mapToJson(mapNode: MapNode): Json = {
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
          key -> registerToJson(node)
      }
      Json.fromFields(fields)
    }
  }

  def listToJson(listNode: ListNode): Json = {
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
        jsons(keyOrder(key)) = registerToJson(node)
    }
    Json.fromValues(jsons)
  }

  // ToDo: implement conflict resolver
  def registerToJson(regNode: RegNode): Json = {
    def replicaIdToKey(id: Id): String = s"${id.p}:${id.c}"
    val fields: Map[String, Json] = regNode.values.map {
      case (id, value) =>
        val key = replicaIdToKey(id)
        val v = value match {
          case Val.False => Json.False
          case Val.True => Json.True
          case Val.Null => Json.Null
          case Val.Num(n) => Json.fromBigDecimal(n)
          case Val.Str(s) => Json.fromString(s)
        }
        (key, v)
    }
    Json.obj(fields.toSeq: _*)
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
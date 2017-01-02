import crdt.json.playground.CRDTNodeToJson._
import crdt.json.playground.RegisterConflictResolver.ListAll
import eu.timepit.crjdt.core._
import eu.timepit.crjdt.core.syntax._

val initCmd = doc.downField("key") := "A"

val p0 = Replica.empty("p").applyCmd(initCmd)

p0.document.toJson
//{
//  "key" : "A"
//}
def merge(r1: Replica, r2: Replica): Replica = {
  r1.applyRemoteOps(r2.generatedOps)
}

val q0 = merge(Replica.empty("q"), p0)
q0.document.toJson
//{
//  "key" : "A"
//}
val p1 = p0.applyCmd(doc.downField("key") := "B")
p1.document.toJson
//{
//  "key" : "B"
//}
val q1 = q0.applyCmd(doc.downField("key") := "C")
q1.document.toJson
//{
//  "key" : "C"
//}
val p2 = merge(p1, q1)
p2.document.toJson
//{
//  "key" : "\"B\" / \"C\""
//}
val q2 = merge(q1, p1)
p2.document.toJson
//{
//  "key" : "\"B\" / \"C\""
//}


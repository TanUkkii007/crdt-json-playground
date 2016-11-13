import crdt.json.playground.CRDTNodeToJson._
import eu.timepit.crjdt.core.Replica
import eu.timepit.crjdt.core.syntax._

def merge(r1: Replica, r2: Replica): Replica = {
  r1.applyRemoteOps(r2.generatedOps)
}

val colors = doc.downField("colors")

val initCmd = colors.downField("blue") := "#0000ff"

val p0 = Replica.empty("p").applyCmd(initCmd)
p0.document.toJson
//{
//  "colors" : {
//    "blue" : "#0000ff"
//  }
//}

val q0 = merge(Replica.empty("q"), p0)
q0.document.toJson
//{
//  "colors" : {
//    "blue" : "#0000ff"
//  }
//}


val p1 = p0.applyCmd(colors.downField("red") := "#ff0000")
p1.document.toJson
//{
//  "colors" : {
//    "blue" : "#0000ff",
//    "red" : "#ff0000"
//  }
//}

val q1 = q0
  .applyCmd(colors := `{}`)
  .applyCmd(colors.downField("green") := "#00ff00")

q1.document.toJson
//{
//  "colors" : {
//    "green" : "#00ff00"
//  }
//}

val p2 = merge(p1, q1)
p2.document.toJson
//{
//  "colors" : {
//    "red" : "#ff0000",
//    "green" : "#00ff00"
//  }
//}
val q2 = merge(q1, p1)
q2.document.toJson
//{
//  "colors" : {
//    "green" : "#00ff00",
//    "red" : "#ff0000"
//  }
//}
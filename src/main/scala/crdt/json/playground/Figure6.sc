import crdt.json.playground.CRDTNodeToJson._
import crdt.json.playground.RegisterConflictResolver.ListAll
import eu.timepit.crjdt.core._
import eu.timepit.crjdt.core.syntax._

val list = v("list")
val eggs = v("eggs")
val cmd = (doc := `{}`) `;`
  (let(list) = doc.downField("shopping").iter) `;`
  list.insert("eggs") `;`
  (let(eggs) = list.next) `;`
  eggs.insert("milk") `;`
  list.insert("cheese")

val d = Replica.empty("").applyCmd(cmd).document

d.toJson
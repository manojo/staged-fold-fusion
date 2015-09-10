package lms.util

/**
 * quasi copy-paste of HashMapOps.
 * And a waste of space
 * Reason is that java.util.HashMap is baked as a type for codegen in
 * the lms lib, and we need an immutable updated here (which is not
 * available for java.util.HashMap)
 */

import java.io.PrintWriter
import scala.lms.internal._
import scala.lms.common._
import scala.collection.mutable.Set
import scala.reflect.SourceContext

/**
 * access to the HashMap type
 */
import scala.collection.immutable.HashMap

trait MyHashMapOps extends Base {

  /**
   * implicits for creating Type Manifests
   * new boilerplate after the Manifest -> Typ change
   */
  implicit def hashMapTyp[K: Typ,V: Typ]: Typ[HashMap[K, V]]

  object HashMap {
    def apply[K: Typ, V: Typ]()(implicit pos: SourceContext) = hashmap_new[K, V]()
  }

  implicit class hashmapOpsCls[K: Typ, V: Typ](m: Rep[HashMap[K, V]]) {
    def apply(k: Rep[K])(implicit pos: SourceContext) = hashmap_apply(m, k)

    def +(k: Rep[K], v: Rep[V])(implicit pos: SourceContext) = hashmap_updated(m, k, v)

    def contains(k: Rep[K])(implicit pos: SourceContext) = hashmap_contains(m, k)
    def size(implicit pos: SourceContext) = hashmap_size(m)
//    def values(implicit pos: SourceContext) = hashmap_values(m)
//
//    def keySet(implicit pos: SourceContext) = hashmap_keyset(m)
//    def keys(implicit pos: SourceContext) = hashmap_keys(m)
  }

  def hashmap_new[K: Typ, V: Typ]()(implicit pos: SourceContext): Rep[HashMap[K, V]]
  def hashmap_apply[K: Typ, V: Typ](m: Rep[HashMap[K, V]], k: Rep[K])
                                   (implicit pos: SourceContext): Rep[V]

  def hashmap_updated[K: Typ, V: Typ](m: Rep[HashMap[K, V]], k: Rep[K], v: Rep[V])
                                     (implicit pos: SourceContext): Rep[HashMap[K, V]]

  def hashmap_contains[K: Typ, V: Typ](m: Rep[HashMap[K, V]], i: Rep[K])
                                      (implicit pos: SourceContext): Rep[Boolean]

  def hashmap_size[K: Typ, V: Typ](m: Rep[HashMap[K, V]])
                                  (implicit pos: SourceContext): Rep[Int]

//  def hashmap_values[K: Typ, V: Typ](m: Rep[HashMap[K, V]])
//                                    (implicit pos: SourceContext): Rep[Iterable[V]]

//  def hashmap_keyset[K: Typ, V: Typ](m: Rep[HashMap[K, V]])
//                                    (implicit pos: SourceContext): Rep[Set[K]]
//
//  def hashmap_keys[K: Typ, V: Typ](m: Rep[HashMap[K, V]])
//                                  (implicit pos: SourceContext): Rep[Iterable[K]]
}

trait MyHashMapOpsExp
    extends MyHashMapOps
    with EffectExp
    /** this trait should be mixed in higher up */ with PrimitiveOpsExp
    /** this trait should be mixed in higher up */ with BooleanOpsExp {

  /**
   * implicits for creating Type Manifests
   * new boilerplate after the Manifest -> Typ change
   */
  implicit def hashMapTyp[K: Typ,V: Typ]: Typ[HashMap[K,V]] = {
    implicit val ManifestTyp(mK) = typ[K]
    implicit val ManifestTyp(mV) = typ[V]
    manifestTyp
  }

  abstract class HashMapDef[K: Typ, V: Typ, R: Typ] extends Def[R] {
    val mK = manifest[K]
    val mV = manifest[V]
  }

  case class HashMapNew[K: Typ, V: Typ]() extends HashMapDef[K, V, HashMap[K, V]]
  case class HashMapApply[K: Typ, V: Typ](m: Exp[HashMap[K, V]], k: Exp[K])
    extends HashMapDef[K, V, V]

  case class HashMapUpdate[K: Typ, V: Typ](m: Exp[HashMap[K, V]], k: Exp[K], v: Exp[V])
    extends HashMapDef[K, V, Unit]
  case class HashMapUpdated[K: Typ, V: Typ](m: Exp[HashMap[K, V]], k: Exp[K], v: Exp[V])
    extends HashMapDef[K, V, HashMap[K, V]]

  case class HashMapContains[K: Typ, V: Typ](m: Exp[HashMap[K, V]], i: Exp[K])
    extends HashMapDef[K, V, Boolean]
  case class HashMapSize[K: Typ, V: Typ](m: Exp[HashMap[K, V]])
    extends HashMapDef[K, V, Int]
//  case class HashMapValues[K: Typ, V: Typ](m: Exp[HashMap[K, V]])
//    extends HashMapDef[K, V, Iterable[V]]
//  case class HashMapClear[K: Typ, V: Typ](m: Exp[HashMap[K, V]])
//    extends HashMapDef[K, V, Unit]

//  case class HashMapKeySet[K: Typ, V: Typ](m: Exp[HashMap[K, V]])
//    extends HashMapDef[K, V, Set[K]]
//  case class HashMapKeys[K: Typ, V: Typ](m: Exp[HashMap[K, V]])
//    extends HashMapDef[K, V, Iterable[K]]

  /**
   * creating an immutable HashMap so no need to wrap an effect around it
   */
  def hashmap_new[K: Typ, V: Typ]()(implicit pos: SourceContext) = HashMapNew[K, V]()
  def hashmap_apply[K: Typ, V: Typ](m: Exp[HashMap[K, V]], k: Exp[K])
                                   (implicit pos: SourceContext) =
    HashMapApply(m, k)

  def hashmap_updated[K: Typ, V: Typ](m: Rep[HashMap[K, V]], k: Rep[K], v: Rep[V])
                                     (implicit pos: SourceContext) =
    HashMapUpdated(m, k, v)

  def hashmap_contains[K: Typ, V: Typ](m: Exp[HashMap[K, V]], i: Exp[K])
                                      (implicit pos: SourceContext) =
    HashMapContains(m, i)

  def hashmap_size[K: Typ, V: Typ](m: Exp[HashMap[K, V]])
                                  (implicit pos: SourceContext) =
    HashMapSize(m)

//  def hashmap_values[K: Typ, V: Typ](m: Exp[HashMap[K, V]])
//                                    (implicit pos: SourceContext) =
//     HashMapValues(m)
//
//  def hashmap_keyset[K: Typ, V: Typ](m: Rep[HashMap[K, V]])
//                                    (implicit pos: SourceContext) =
//    HashMapKeySet(m)
//
//  def hashmap_keys[K: Typ, V: Typ](m: Rep[HashMap[K, V]])
//                                  (implicit pos: SourceContext) =
//    HashMapKeys(m)

}

trait MyBaseGenHashMapOps extends GenericNestedCodegen {
  val IR: MyHashMapOpsExp
  import IR._

}

trait MyScalaGenHashMapOps extends MyBaseGenHashMapOps with ScalaGenEffect {
  val IR: MyHashMapOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case m @ HashMapNew() => emitValDef(sym, src"collection.immutable.HashMap[${m.mK},${m.mV}]()")
    case HashMapApply(m, k) => emitValDef(sym, src"$m($k)")

    case e @ HashMapUpdated(m, k, v) => emitValDef(
      sym,
      src"$m.updated($k, $v).asInstanceOf[collection.immutable.HashMap[${e.mK},${e.mV}]]"
    )

    case HashMapContains(m, i) => emitValDef(sym, src"$m.contains($i)")
    case HashMapSize(m) => emitValDef(sym, src"$m.size")
//    case HashMapValues(m) => emitValDef(sym, src"$m.values")

//    case HashMapKeySet(m) => emitValDef(sym, src"$m.keySet")
//    case HashMapKeys(m) => emitValDef(sym, src"$m.keys")

    case _ => super.emitNode(sym, rhs)
  }
}

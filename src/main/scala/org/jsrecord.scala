import shapeless._
import record._
import labelled.FieldType
import syntax.singleton._

import scala.scalajs.js
import js.annotation.ScalaJSDefined

package object jsrecord {

  /**
    * A generic "plain configuration object" type.
    *
    * For example, this type:
    * {{{
    * JSRecord[Record.`'foo->Int, 'bar->String`.T]
    * }}}
    * represents the following structure:
    * {{{
    * {
    *   foo: <number>,
    *   bar: <string>
    * }
    * }}}
    *
    * This representation should allow us to express some constraints on types
    * that aren't easily translated to `ScalaJSDefined` traits. For example,
    * react-redux's `connect`.
    */
  @ScalaJSDefined
  trait JSRecord[M] extends js.Object

  object JSRecord {

    sealed trait ValidRecord[M] {
      def apply(m: M): JSRecord[M]
    }

    object ValidRecord {

      def apply[M](implicit vr: ValidRecord[M]) = vr

      implicit def validRecord[M <: HList, FS <: HList, KS <: HList](
        implicit
          fs: ops.record.Fields.Aux[M, FS],
          t: ops.hlist.ToTraversable.Aux[FS, List, (Symbol, Any)],
          ks: ops.record.Keys.Aux[M, KS],
          d: IsDistinctConstraint[KS]
      ): ValidRecord[M] = new ValidRecord[M] {
        def apply(m: M) = {
          var res = js.Dynamic.literal()
          for {
            (k, v) <- m.fields.toList
          } res.updateDynamic(k.name)(v.asInstanceOf[js.Any])
          res.asInstanceOf[JSRecord[M]]
        }
      }
    }

    def apply[M](m: M)(implicit vr: ValidRecord[M]): JSRecord[M] =
      vr(m)
  }

  implicit class JSRecordOps[M <: HList](self: JSRecord[M]) {
    def get(k: Witness)(implicit
      s: ops.record.Selector[M, k.T],
      ev: k.T <:< Symbol
    ): s.Out = {
      self.asInstanceOf[js.Dynamic].selectDynamic(k.value.name).asInstanceOf[s.Out]
    }
  }

  // val x = JSRecord(
  //   'foo ->> 123 ::
  //   'bar ->> "hello" ::
  //   HNil
  // )

  // def say(x: Int) = x + " is an Int"
  // def say(x: String) = x + " is a String"

  // println(say(x.get(Witness('foo))))
  // println(say(x.get(Witness('bar))))

}

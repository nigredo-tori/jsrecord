package jsrecord

import scala.language.dynamics

import shapeless._
import record._
import labelled.{ FieldType, field }
import syntax.singleton._

import scala.scalajs.js
import js.annotation.ScalaJSDefined

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
  *
  * Invariants:
  * - Field values can't be [[js.undefined]] - this this doesn't mix well with
  *   ES6 object literals
  */
@js.native
// Dynamic is not implemented directly, but rather provided by JSRecordOps!
sealed trait JSRecord[M <: HList] extends js.Object with scala.Dynamic

object JSRecord {

  sealed trait ValidRecord[M <: HList] {
    def toJS(m: M): JSRecord[M]
    def fromJS(jm: JSRecord[M]): M
  }

  object ValidRecord {

    def apply[M <: HList](implicit vr: ValidRecord[M]) = vr

    implicit def validRecord[M <: HList, FS <: HList, KS <: HList](
      implicit
        fields: ops.record.Fields.Aux[M, FS],
      toTraversable: ops.hlist.ToTraversable.Aux[FS, List, (String, Any)],
      unsafeFromDynamic: UnsafeFromDynamic[M],
      keys: ops.record.Keys.Aux[M, KS],
      distinct: IsDistinctConstraint[KS]
    ): ValidRecord[M] = new ValidRecord[M] {
      def toJS(m: M): JSRecord[M] = {
        var res = js.Dynamic.literal()
        for {
          (k, v) <- m.fields.toList
          // Don't create fields with `undefined` as a value
          if (!js.isUndefined(v))
            } res.updateDynamic(k)(v.asInstanceOf[js.Any])
        res.asInstanceOf[JSRecord[M]]
      }

      def fromJS(jm: JSRecord[M]): M = {
        unsafeFromDynamic(jm.asInstanceOf[js.Dynamic])
      }
    }
  }

  def apply[M <: HList](m: M)(implicit vr: ValidRecord[M]): JSRecord[M] =
    vr.toJS(m)

  implicit def toJSRecordOps[M <: HList](self: JSRecord[M]): JSRecordOps[M] =
    new JSRecordOps(self)

  class Companion[M <: HList](implicit val ev: ValidRecord[M]) extends RecordArgs {

    type T = JSRecord[M]

    // TODO: unlabeled syntax
    // TODO: defaults
    def applyRecord[KS <: HList, R <: HList, M0 <: HList](args: R)(
      implicit
        mapper: ops.hlist.Mapper.Aux[impl.stripArgs.type, R, M0],
      align: ops.hlist.Align[M0, M]
    ): T = JSRecord(align(mapper(args)))
  }
}

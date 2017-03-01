package jsrecord

import scala.language.dynamics
import scala.language.implicitConversions

import jsrecord.operation.{ UnsafeFromDynamic, StripArgs }

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

    type Fields <: HList
    type Keys <: HList
    type Values <: HList

    val keys: ops.record.Keys.Aux[M, Keys]
    val distinct: IsDistinctConstraint[Keys]
    val values: ops.record.Values.Aux[M, Values]
    val fields: ops.record.Fields.Aux[M, Fields]
    val toList: ops.hlist.ToTraversable.Aux[Fields, List, (String, Any)]
    val unsafeFromDynamic: UnsafeFromDynamic[M]
  }

  object ValidRecord {

    type Aux[M <: HList, FS <: HList, KS <: HList, VS <: HList] =
      ValidRecord[M] {
        type Fields = FS
        type Keys = KS
        type Values = VS
      }

    def apply[M <: HList](implicit vr: ValidRecord[M]) = vr

    implicit def validRecord[
      M <: HList, FS <: HList, KS <: HList, VS <: HList
    ](implicit
      fields0: ops.record.Fields.Aux[M, FS],
      keys0: ops.record.Keys.Aux[M, KS],
      distinct0: IsDistinctConstraint[KS],
      values0: ops.record.Values.Aux[M, VS],
      toList0: ops.hlist.ToTraversable.Aux[FS, List, (String, Any)],
      unsafeFromDynamic0: UnsafeFromDynamic[M]
    ): ValidRecord.Aux[M, FS, KS, VS] = new ValidRecord[M] {

      type Fields = FS
      type Keys = KS
      type Values = VS

      val fields = fields0
      val keys = keys0
      val distinct = distinct0
      val values = values0
      val toList = toList0
      val unsafeFromDynamic = unsafeFromDynamic0
    }
  }

  def toJS[M <: HList](m: M)(implicit rec: ValidRecord[M]): JSRecord[M] = {
    var res = js.Dynamic.literal()
    for {
      (k, v) <- rec.toList(rec.fields(m))
      // Don't create fields with `undefined` as a value
      if (!js.isUndefined(v))
        } res.updateDynamic(k)(v.asInstanceOf[js.Any])
    res.asInstanceOf[JSRecord[M]]
  }

  def fromJS[M <: HList](jm: JSRecord[M])(implicit rec: ValidRecord[M]): M = {
    rec.unsafeFromDynamic(jm.asInstanceOf[js.Dynamic])
  }

  def apply[M <: HList: ValidRecord](m: M)(): JSRecord[M] =
    toJS(m)

  implicit def toJSRecordOps[M <: HList](self: JSRecord[M]): JSRecordOps[M] =
    new JSRecordOps(self)

  class Companion[M <: HList] extends RecordArgs {

    type T = JSRecord[M]

    // TODO: unlabeled syntax
    // TODO: defaults
    def applyRecord[R <: HList, M0 <: HList](args: R)(
      implicit
        rec: ValidRecord[M],
      mapper: ops.hlist.Mapper.Aux[StripArgs.type, R, M0],
      align: ops.hlist.Align[M0, M]
    ): T = toJS(align(mapper(args)))

    def unapply[VS <: HList, VST](r: JSRecord[M])(
      implicit
        rec: ValidRecord.Aux[M, _, _, VS],
      toTuple: ops.hlist.Tupler.Aux[VS, VST]
    ): Some[VST] =
      Some(toTuple(rec.values(fromJS(r))))
  }
}

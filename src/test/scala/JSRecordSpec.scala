package jsrecord

import org.scalatest.{ FunSpec, Matchers }

import scala.scalajs.js

import shapeless._
import shapeless.record.Record
import shapeless.syntax.singleton._
import shapeless.test._

import scalaz.Liskov
import Liskov.<~<

class JSRecordSpec extends FunSpec with Matchers {

  describe("apply(Record)") {
    it("should construct empty object for empty record") {
      val x = JSRecord[HNil](HNil)
      typed[JSRecord[Record.` `.T]](x)
      assertJsEq(x, js.Dynamic.literal())
    }
    it("should construct object of expected structure") {
      val x = JSRecord(
        "foo" ->> 123 ::
          "bar" ->> "hello" ::
          HNil
      )
      typed[JSRecord[Record.`"foo" -> Int, "bar" -> String`.T]](x)
      assertJsEq(x, js.Dynamic.literal(foo = 123, bar = "hello"))
    }
    it("shouldn't add undefined fields") {
      val x = JSRecord(
        "foo" ->> (123: js.UndefOr[Int]) ::
          "bar" ->> (js.undefined: js.UndefOr[String]) ::
          HNil
      )
      typed[JSRecord[Record.`"foo" -> js.UndefOr[Int], "bar" -> js.UndefOr[String]`.T]](x)
      assertJsEq(x, js.Dynamic.literal(foo = 123))
    }
  }

  describe("get") {
    it("should work for existing keys") {
      val x = JSRecord(
        "foo" ->> (123: js.UndefOr[Int]) ::
          "bar" ->> (js.undefined: js.UndefOr[String]) ::
          "baz" ->> 12.34 ::
          HNil
      )

      val foo = x.get("foo")
      typed[js.UndefOr[Int]](foo)
      foo should equal (123)

      val bar = x.get("bar")
      typed[js.UndefOr[String]](bar)
      assert(js.isUndefined(bar))

      val baz = x.get("baz")
      typed[Double](baz)
      baz should equal (12.34)
    }

    it("should fail for missing keys") {
      val x = JSRecord("foo" ->> 123 :: HNil)
      illTyped("""x.get("bar")""")
    }
  }

  describe("field access") {
    it("should work for existing keys") {
      val x = JSRecord(
        "foo" ->> (123: js.UndefOr[Int]) ::
          "bar" ->> (js.undefined: js.UndefOr[String]) ::
          "baz" ->> 12.34 ::
          HNil
      )

      typed[js.UndefOr[Int]](x.foo)
      x.foo should equal (123)

      typed[js.UndefOr[String]](x.bar)
      assert(js.isUndefined(x.bar))

      typed[Double](x.baz)
      x.baz should equal (12.34)
    }

    it("should fail for missing keys") {
      val x = JSRecord("foo" ->> 123 :: HNil)
      illTyped("""x.bar""")
    }
  }

  describe("toRecord") {
    it("should work for existing keys") {
      val rec0 = "foo" ->> 123 ::
        "bar" ->> "hello" ::
        HNil
      val x = JSRecord(rec0)
      val rec = x.toRecord
      typed[Record.`"foo" -> Int, "bar" -> String`.T](rec)
      rec should equal(rec0)
    }
  }

  describe("copy") {
    it("should work for existing keys") {
      val x = JSRecord(
        "foo" ->> 123 ::
          "bar" ->> "456" ::
          HNil
      )
      val y = x.copy(foo = 789)
      typed[JSRecord[Record.`"foo" -> Int, "bar" -> String`.T]](y)
      y.foo should equal(789)
      y.bar should equal("456")
    }

    it("should fail for missing or ill-typed fields") {
      val x = JSRecord(
        "foo" ->> 123 ::
          HNil
      )

      illTyped("""x.copy(bar = 123)""")
      illTyped("""x.copy(foo = "abc")""")
    }
  }

  describe("Companion") {
    it("should have corresponding T") {
      val c = new JSRecord.Companion[Record.`"foo" -> Int`.T]

      typed[JSRecord[Record.`"foo" -> Int`.T]](null: c.T)
    }
    describe("apply") {
      it("should work for correct named arguments") {
        val R = new JSRecord.Companion[
          Record.`"foo" -> Int, "bar" -> Int, "baz" -> String`.T
        ]

        val r = R(foo = 12, baz = "hi", bar = 34)

        typed[R.T](r)
        r.foo should equal (12)
        r.bar should equal (34)
        r.baz should equal ("hi")
      }
      it("should fail for incorrect argument list") {
        val R = new JSRecord.Companion[
          Record.`"foo" -> Int, "bar" -> String`.T
        ]

        // Base case should work
        R(foo = 123, bar = "foo")

        illTyped("""R(foo = 123)""")
        illTyped("""R(foo = 1, bar = 2)""")
        illTyped("""R(foo = 1, bar = "abc", baz = 2)""")
      }
    }
    describe("unapply") {
      it("should work correctly") {
        val R = new JSRecord.Companion[
          Record.`"foo" -> Int, "bar" -> Int, "baz" -> String`.T
        ]

        val r = R(foo = 12, baz = "hi", bar = 34)

        r match {
          case R(foo, bar, baz) =>
            foo should equal (r.foo)
            bar should equal (r.bar)
            baz should equal (r.baz)
        }
      }
    }
    describe("as") {
      it("should handle a trivial case") {
        val R = new JSRecord.Companion[
          Record.`"foo" -> Int, "bar" -> String`.T
        ]

        val r0 = R(foo = 123, bar = "hi")

        // Identity
        val r1 = r0.as[R.T]
        typed[R.T](r1)
        assertJsEq(r1, r0)

        // Alignment
        val r2 = JSRecord(
          "bar" ->> "hi" ::
            "foo" ->> 123 ::
            HNil
        ).as[R.T]
        typed[R.T](r2)
        assertJsEq(r2, r0)
      }

      it("shouldn't allow extra fields") {
        val R = new JSRecord.Companion[Record.`"foo" -> Int`.T]

        val x = JSRecord("foo" ->> 123 :: "bar" ->> "hi" :: HNil)
        illTyped("""x.as[R.T]""")
      }

      it("should allow incomplete specification") {
        val R = new JSRecord.Companion[
          Record.`"foo" -> js.UndefOr[Int], "bar" -> js.UndefOr[String]`.T
        ]

        val r1 = JSRecord("foo" ->> (123: js.UndefOr[Int]) :: HNil).as[R.T]
        typed[R.T](r1)
        assertJsEq(r1, js.Dynamic.literal(foo = 123))
      }

      it("should handle subtyping") {

        class A
        class B extends A
        class C extends B

        val R = new JSRecord.Companion[
          Record.`"foo" -> B`.T
        ]

        // a field is a subtype of required type
        val x = JSRecord("foo" ->> new C :: HNil)
        typed[R.T](x.as[R.T])

        // a field is a supertype of required type
        val y = JSRecord("foo" ->> new A :: HNil)
        illTyped("""y.as[R.T]""")
      }

      it("should handle Liskov") {

        case class X[T](v: T)

        type A = X[Object]
        type B = X[String]

        implicit val lab: B <~< A = Liskov.force

        val a = X[Object](new Object())
        val b = X[String]("hi")

        val RA = new JSRecord.Companion[
          Record.`"foo" -> A`.T
        ]

        val ra = RA(foo = a)

        val RB = new JSRecord.Companion[
          Record.`"foo" -> B`.T
        ]

        val rb = RB(foo = b)

        typed[RA.T](rb.as[RA.T])
        illTyped("""ra.as[RB.T]""")

        // Should provide Liskov instance
        implicitly[RB.T <~< RA.T]
        illTyped("""`implicitly[RA.T <~< RB.T]`""")
      }
    }
  }

  // Check that two values have identical JS representation
  def assertJsEq(a: js.Any, b: js.Any): Unit = (a, b) match {
    case (a0: js.Object, b0: js.Object) =>
      assert(js.Object.getPrototypeOf(a0) == js.Object.getPrototypeOf(b0))
      val a = a0.asInstanceOf[js.Dictionary[js.Any]]
      val b = b0.asInstanceOf[js.Dictionary[js.Any]]
      assert(a.keys.toSet == b.keys.toSet)
      a.keys.foreach( k => assertJsEq(a(k), b(k)) )
    case _ => a == b
  }

  def debug(x: js.Any) = {
    js.Dynamic.global.console.log(
      js.Dynamic.global.JSON.stringify(
        x
      )
    )
  }
}

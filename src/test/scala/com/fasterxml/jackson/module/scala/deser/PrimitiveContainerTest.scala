package com.fasterxml.jackson.module.scala.deser

import com.fasterxml.jackson.databind.JsonMappingException
import com.fasterxml.jackson.databind.annotation.JsonDeserialize
import com.fasterxml.jackson.databind.exc.InvalidFormatException
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

object PrimitiveContainerTest
{
  case class OptionSeqInt(os: Option[Seq[Int]])

  case class OptionInt(value: Option[Int])
  case class AnnotatedOptionInt(@JsonDeserialize(contentAs = classOf[java.lang.Integer]) value: Option[Int])
  case class OptionLong(value: Option[Long])
  case class AnnotatedOptionLong(@JsonDeserialize(contentAs = classOf[java.lang.Long]) value: Option[Long])

  case class AnnotatedHashKeyLong(@JsonDeserialize(keyAs = classOf[java.lang.Long]) value: Map[Long, String])
  case class AnnotatedHashValueLong(@JsonDeserialize(contentAs = classOf[java.lang.Long]) value: Map[String, Long])
}

@RunWith(classOf[JUnitRunner])
class PrimitiveContainerTest extends DeserializationFixture
{
  import PrimitiveContainerTest._

  behavior of "Primitive Containers"

  it should "support deserializing primitives" in { f =>
    val value = f.readValue[OptionInt]("""{"value":1}""")
    value.value shouldBe Some(1)
  }

  it should "support primitive conversions in" in { f =>
    val value = f.readValue[AnnotatedOptionInt]("""{"value":"1"}""")
    value.value shouldBe Some(1)
  }

  it should "support type widening"  in { f =>
    val value = f.readValue[AnnotatedOptionLong]("""{"value":1}""")
    value.value.get shouldBe 1L
  }

  it should "enforce type constraints"  in { f =>
    val thrown = intercept[JsonMappingException] {
      f.readValue[AnnotatedOptionInt]("""{"value":9223372036854775807}""").value.get
    }
    thrown.getMessage should startWith ("Numeric value (9223372036854775807) out of range")
  }

  it should "support map keys" in { f =>
    val value = f.readValue[AnnotatedHashKeyLong]("""{"value":{"1":"one"}}""")
    value.value should contain key 1L
    value.value(1L) shouldBe "one"
  }

  it should "support map values" in { f =>
    val value = f.readValue[AnnotatedHashValueLong]("""{"value":{"key": "1"}}""")
    value.value should contain key "key"
    value.value("key") shouldBe 1L
  }

  it should "throw if type not convertible in Option" in { f =>
    val thrown = intercept[InvalidFormatException] {
      f.readValue[Option[Int]](""""a cat"""")
    }
    thrown.getMessage should startWith ("Can not construct instance of")
  }

  it should "throw if type not convertible in Array" in { f =>
    val thrown = intercept[InvalidFormatException] {
      f.readValue[Array[Int]]("""["value"]""")
    }
    thrown.getMessage should startWith ("Can not construct instance of")
  }

  it should "throw if type not convertible in Seq" in { f =>
    val thrown = intercept[InvalidFormatException] {
      f.readValue[Seq[Int]]("""["value"]""")
    }
    thrown.getMessage should startWith ("Can not construct instance of")
  }

  it should "throw if type not convertible in Map" in { f =>
    val thrown = intercept[InvalidFormatException] {
      f.readValue[Map[String, Int]]("""{"value":"a cat"}""")
    }
    thrown.getMessage should startWith ("Can not construct instance of")
  }

// See #243, cannot be picky until we pick up scala reflection.
//  it should "throw if type not convertible in OptionInt" in { f =>
//    val thrown = intercept[InvalidFormatException] {
//      f.readValue[OptionInt]("""{"value":"a cat"}""")
//    }
//    thrown.getMessage should startWith ("Can not construct instance of")
//  }
//
//  it should "throw if type not convertible in SeqInt" in { f =>
//    val thrown = intercept[InvalidFormatException] {
//      f.readValue[OptionSeqInt]("""{"os":["a cat"]}""")
//    }
//    thrown.getMessage should startWith ("Can not construct instance of")
//  }
}

package com.fasterxml.jackson.module.scala.modifiers

import java.lang.reflect.Type

import com.fasterxml.jackson.databind.JavaType
import com.fasterxml.jackson.databind.`type`.{ReferenceType, TypeBindings, TypeFactory, TypeModifier}
import com.fasterxml.jackson.module.scala.JacksonModule

private object OptionTypeModifier extends TypeModifier with GenTypeModifier {
  def BASE = classOf[Option[Any]]

  override def modifyType(originalType: JavaType, jdkType: Type, context: TypeBindings, typeFactory: TypeFactory) = {
    if (classObjectFor(jdkType).exists(BASE.isAssignableFrom) && !originalType.isMapLikeType && originalType.containedTypeCount <= 1) {
      val valType = originalType.containedTypeOrUnknown(0)
      ReferenceType.upgradeFrom(originalType, valType)
    } else originalType
  }
}

trait OptionTypeModifierModule extends JacksonModule {
  this += OptionTypeModifier
}

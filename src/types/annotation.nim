import std/tables
import std/options

type 
  Annotation* = object
    name*: string
    properties*: Table[string, Option[string]]
  Annotations* = distinct Table[string, Annotation]

proc hasAnnotation*(annotations: Annotations, name: string): bool =
  return Table[string, Annotation](annotations).hasKey(name)

proc getAnnotation*(annotations: Annotations, name: string): Option[Annotation] =
  if annotations.hasAnnotation(name):
    return some(Table[string, Annotation](annotations)[name])
  else:
    return none(Annotation)

proc hasProperty*(annotation: Annotation, propertyName: string): bool =
  return annotation.properties.hasKey(propertyName)

proc hasProperty*(annotation: Option[Annotation], propertyName: string): bool =
  if annotation.isSome:
    return annotation.get().hasProperty(propertyName)
  else:
    return false

proc getProperty*(annotation: Annotation, propertyName: string): Option[string] =
  if annotation.properties.hasKey(propertyName):
    return annotation.properties[propertyName]
  else:
    return none(string)

proc getProperty*(annotation: Option[Annotation], propertyName: string): Option[string] =
  if annotation.isSome:
    return annotation.get().getProperty(propertyName)
  else:
    return none(string)

proc `[]`*(annotations: Annotations, name: string): Option[Annotation] =
  if annotations.hasAnnotation(name):
    return annotations.getAnnotation(name)
  else:
    return none(Annotation)

proc `[]`*(annotation: Annotation, propertyName: string): Option[string] =
  return annotation.getProperty(propertyName)

proc `[]`*(annotation: Option[Annotation], propertyName: string): Option[string] =
  return annotation.getProperty(propertyName)

proc `[]`*(annotations: Annotations, name, propertyName: string): Option[string] =
  if annotations.hasAnnotation(name):
    return annotations.getAnnotation(name).getProperty(propertyName)
  else:
    return none(string)
import std/tables

type
  TypeKind* = enum
    TkPrimitive
    TkPointer
    TkROPointer
    TkMeta
    TkStruct
    TkArray

  Primitive* = enum
    Int
    Int8
    Int16
    Int32
    Int64
    UInt
    UInt8
    UInt16
    UInt32
    UInt64
    Float
    Float32
    Float64
    Bool
    Char
    String
    Void
    CString

  MetaKind* = enum
    MkVarArgs
    MkCVarArgs
    MkNamedType
    MkToInfer
    MkResolveError
    MkAnyInt
    MkAnyFloat
    MkAnyUInt
    MkAnyPointer # type for nil literals

  Visibility* = enum
    Public
    Private

  StructMember* = object
    name*: string
    typ*: Type
    visibility*: Visibility

  StructType* = object
    name*: string
    members*: Table[string, StructMember]

  ArrayType* = object
    typ*: Type
    size*: int

  Type* = ref object
    hasAddress*: bool = false # when type is from a variable
    fromRO*: bool = false # when type is from a ro variable
    case kind*: TypeKind
    of TkPrimitive:
      primitive*: Primitive
    of TkPointer, TkROPointer:
      pointerTo*: Type
    of TkMeta:
      case metaKind*: MetaKind
      of MkVarArgs, MkCVarArgs, MkToInfer, MkAnyInt, MkAnyFloat, MkAnyUInt, MkAnyPointer:
        discard
      of MkNamedType, MkResolveError:
        name*: string
    of TkStruct:
      structType*: StructType
    of TkArray:
      arrayType*: ArrayType

proc newPointerType*(t: Type, isConst: bool): Type =
  ## Creates a new pointer type
  if isConst:
    result = Type(kind: TkROPointer, pointerTo: t)
  else:
    result = Type(kind: TkPointer, pointerTo: t)

proc newArrayType*(t: Type, size: int): Type =
  ## Creates a new array type
  result = Type(kind: TkArray, arrayType: ArrayType(typ: t, size: size))

proc `==`*(a, b: Type): bool =
  ## Compares two Type objects for equality
  if a.kind != b.kind:
    return false

  case a.kind
  of TkPrimitive:
    result = a.primitive == b.primitive
  of TkPointer, TkROPointer:
    if a.pointerTo.isNil or b.pointerTo.isNil:
      result = a.pointerTo.isNil and b.pointerTo.isNil
    else:
      result = a.pointerTo == b.pointerTo
  of TkMeta:
    if a.metaKind != b.metaKind:
      return false

    case a.metaKind
    of MkVarArgs, MkCVarArgs, MkToInfer, MkAnyInt, MkAnyFloat, MkAnyUInt, MkAnyPointer:
      result = true # These are equal if the metaKind is the same (already checked)
    of MkNamedType, MkResolveError:
      result = a.name == b.name
  of TkStruct:
    if a.structType.name != b.structType.name:
      # this is enough to distinguish structs since name is the canonical name and is unique
      return false
    result = true
  of TkArray:
    if a.arrayType.size != b.arrayType.size:
      return false
    result = a.arrayType.typ == b.arrayType.typ

proc `$`*(t: Type): string =
  ## Returns a string representation of a Type
  case t.kind
  of TkPrimitive:
    result = $t.primitive
  of TkPointer:
    if t.pointerTo.isNil:
      result = "Pointer(nil)"
    else:
      result = "*" & $t.pointerTo
  of TkROPointer:
    if t.pointerTo.isNil:
      result = "ConstPointer(nil)"
    else:
      result = "ro*" & $t.pointerTo
  of TkMeta:
    case t.metaKind
    of MkVarArgs:
      result = "VarArgs"
    of MkCVarArgs:
      result = "CVarArgs"
    of MkToInfer:
      result = "ToInfer"
    of MkNamedType:
      result = "Named(" & t.name & ")"
    of MkResolveError:
      result = "ResolveError(" & t.name & ")"
    of MkAnyInt:
      result = "AnyInt"
    of MkAnyFloat:
      result = "AnyFloat"
    of MkAnyUInt:
      result = "AnyUInt"
    of MkAnyPointer:
      result = "AnyPointer"
  of TkStruct:
    result = "struct " & t.structType.name
  of TkArray:
    result = "array[" & $t.arrayType.typ & ", " & $t.arrayType.size & "]"

import std/strutils

proc asTree*(t: Type, indent: int = 0): string =
  ## Pretty print Type as a tree
  let indentStr = "  ".repeat(indent)
  if t.isNil:
    return indentStr & "<nil type>\n"
  # Add hasAddress and fromRO metadata
  result.add indentStr & "[hasAddress=" & $t.hasAddress & ", fromRO=" & $t.fromRO & "]\n"
  case t.kind
  of TkPrimitive:
    result.add indentStr & "Primitive: " & $t.primitive & "\n"
  of TkPointer:
    result.add indentStr & "Pointer:\n"
    result.add asTree(t.pointerTo, indent + 1)
  of TkROPointer:
    result.add indentStr & "ROPointer:\n"
    result.add asTree(t.pointerTo, indent + 1)
  of TkMeta:
    result.add indentStr & "Meta: " & $t.metaKind
    case t.metaKind
    of MkNamedType, MkResolveError:
      result.add " (" & t.name & ")"
      result.add "\n"
    of MkVarArgs:
      result.add "\n" & indentStr & "  VarArgs\n"
    of MkCVarArgs:
      result.add "\n" & indentStr & "  CVarArgs\n"
    of MkToInfer:
      result.add "\n" & indentStr & "  ToInfer\n"
    of MkAnyInt:
      result.add "\n" & indentStr & "  AnyInt\n"
    of MkAnyFloat:
      result.add "\n" & indentStr & "  AnyFloat\n"
    of MkAnyUInt:
      result.add "\n" & indentStr & "  AnyUInt\n"
    of MkAnyPointer:
      result.add "\n" & indentStr & "  AnyPointer\n"
  of TkStruct:
    result.add indentStr & "Struct: " & t.structType.name & "\n"
  of TkArray:
    result.add indentStr & "Array:\n"
    result.add indentStr & "  Type: " & $t.arrayType.typ & "\n"
    result.add indentStr & "  Size: " & $t.arrayType.size & "\n"
import std/tables

type
  TypeKind* = enum
    TkPrimitive
    TkPointer
    TkROPointer
    TkMeta
    TkStruct

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

proc newPointerType*(t: Type, isConst: bool): Type =
  ## Creates a new pointer type
  if isConst:
    result = Type(kind: TkROPointer, pointerTo: t)
  else:
    result = Type(kind: TkPointer, pointerTo: t)

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
      return false
    if a.structType.members.len != b.structType.members.len:
      return false
    for name, am in a.structType.members:
      if not b.structType.members.hasKey(name):
        return false
      let bm = b.structType.members[name]
      if am.name != bm.name or am.visibility != bm.visibility or am.typ != bm.typ:
        return false
    result = true

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
      result = "Unresolved(" & t.name & ")"
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
    result = "struct " & t.structType.name & " {"
    for _, m in t.structType.members:
      result.add($m.visibility & " " & m.name & ": " & $m.typ)
      result.add(", ")
    result.add("}")

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
    for _, m in t.structType.members:
      result.add indentStr & "  Member: " & $m.visibility & " " & m.name & "\n"
      result.add asTree(m.typ, indent + 2)

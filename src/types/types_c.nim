import std/options
import std/tables
import ./types

type
  CTypeKind* = enum
    CkPrimitive
    CkStruct
    CkUnion
    CkEnum
    CkTypedef
    CkPointer
    CkConstPointer

  CPrimitive* = enum
    Int8T
    Int16T
    Int32T
    Int64T
    UInt8T
    UInt16T
    UInt32T
    UInt64T
    FloatT
    DoubleT
    BoolT
    CharT
    VoidT
    CharPtrT
    VarArgsT

  CStructMember* = object
    name*: string
    ctype*: CType

  CStructType* = object
    name*: string
    members*: Table[string, CStructMember]

  CType* = ref object
    case kind*: CTypeKind
    of CkPrimitive:
      primitive*: CPrimitive
    of CkStruct:
      structType*: CStructType
    of CkUnion, CkEnum, CkTypedef:
      discard # TODO
    of CkPointer, CkConstPointer:
      ctype*: CType

proc `==`*(a, b: CType): bool =
  ## Compares two CType objects for equality
  if a.kind != b.kind:
    return false

  case a.kind
  of CkPrimitive:
    result = a.primitive == b.primitive
  of CkStruct:
    if a.structType.name != b.structType.name:
      return false
    if a.structType.members.len != b.structType.members.len:
      return false
    for key, value in a.structType.members:
      let am = a.structType.members[key]
      let bm = b.structType.members[key]
      if am.name != bm.name:
        return false
      if am.ctype.isNil or bm.ctype.isNil:
        if not (am.ctype.isNil and bm.ctype.isNil):
          return false
      elif am.ctype != bm.ctype:
        return false
    result = true
  of CkUnion, CkEnum, CkTypedef:
    result = true # To be implemented when these types are fully defined
  of CkPointer, CkConstPointer:
    if a.ctype.isNil or b.ctype.isNil:
      result = a.ctype.isNil and b.ctype.isNil
    else:
      result = a.ctype == b.ctype

proc `$`*(t: CType): string =
  ## Returns a string representation of a CType
  case t.kind
  of CkPrimitive:
    result = $t.primitive
  of CkStruct:
    result = "struct " & t.structType.name & " {"
    for i, m in t.structType.members:
      result.add(m.name & ": " & (if m.ctype.isNil: "nil" else: $m.ctype))
      result.add(", ")
    result.add("}")
  of CkUnion:
    result = "union" # To be expanded
  of CkEnum:
    result = "enum" # To be expanded
  of CkTypedef:
    result = "typedef" # To be expanded
  of CkPointer:
    if t.ctype.isNil:
      result = "pointer(nil)"
    else:
      result = "pointer(" & $t.ctype & ")"
  of CkConstPointer:
    if t.ctype.isNil:
      result = "const pointer(nil)"
    else:
      result = "const pointer(" & $t.ctype & ")"

proc getCString*(t: CType): string =
  ## Returns the C string representation of a CType
  case t.kind
  of CkPrimitive:
    case t.primitive
    of Int8T:
      result = "int8_t"
    of Int16T:
      result = "int16_t"
    of Int32T:
      result = "int32_t"
    of Int64T:
      result = "int64_t"
    of UInt8T:
      result = "uint8_t"
    of UInt16T:
      result = "uint16_t"
    of UInt32T:
      result = "uint32_t"
    of UInt64T:
      result = "uint64_t"
    of FloatT:
      result = "float"
    of DoubleT:
      result = "double"
    of BoolT:
      result = "_Bool"
    of CharT:
      result = "char"
    of VoidT:
      result = "void"
    of CharPtrT:
      result = "char*"
    of VarArgsT:
      result = "..."
  of CkStruct:
    result = "struct " & t.structType.name
  of CkUnion:
    result = "union" # To be expanded
  of CkEnum:
    result = "enum" # To be expanded
  of CkTypedef:
    result = "typedef" # To be expanded
  of CkPointer:
    if t.ctype.isNil:
      result = "void*"
    else:
      result = getCString(t.ctype) & "*"
  of CkConstPointer:
    if t.ctype.isNil:
      result = "const void*"
    else:
      result = getCString(t.ctype) & " const*"

proc toCType*(t: Type): Option[CType] =
  ## Converts a Type to a CType
  ## Returns none if the type cannot be represented in C
  case t.kind
  of TkPrimitive:
    var ctype = CType(kind: CkPrimitive)
    case t.primitive
    of Int, Int64:
      ctype.primitive = Int64T
    of Int8:
      ctype.primitive = Int8T
    of Int16:
      ctype.primitive = Int16T
    of Int32:
      ctype.primitive = Int32T
    of UInt, UInt64:
      ctype.primitive = UInt64T
    of UInt8:
      ctype.primitive = UInt8T
    of UInt16:
      ctype.primitive = UInt16T
    of UInt32:
      ctype.primitive = UInt32T
    of Float, Float64:
      ctype.primitive = DoubleT
    of Float32:
      ctype.primitive = FloatT
    of Bool:
      ctype.primitive = BoolT
    of Char:
      ctype.primitive = CharT
    of Void:
      ctype.primitive = VoidT
    of CString:
      ctype.primitive = CharPtrT
    of String:
      return none(CType) # String type is not implemented yet
    result = some(ctype)
  of TkPointer:
    if t.pointerTo.isNil:
      result = some(CType(kind: CkPointer, ctype: nil))
    else:
      var innerCType = new CType
      let innerCTypeOpt = toCType(t.pointerTo)
      if innerCTypeOpt.isNone:
        return none(CType)
      innerCType = innerCTypeOpt.get()
      result = some(CType(kind: CkPointer, ctype: innerCType))
  of TkROPointer:
    if t.pointerTo.isNil:
      result = some(CType(kind: CkConstPointer, ctype: nil))
    else:
      var innerCType = new CType
      let innerCTypeOpt = toCType(t.pointerTo)
      if innerCTypeOpt.isNone:
        return none(CType)
      innerCType = innerCTypeOpt.get()
      result = some(CType(kind: CkConstPointer, ctype: innerCType))
  of TkMeta:
    case t.metaKind
    of MkCVarArgs:
      var ctype = CType(kind: CkPrimitive, primitive: VarArgsT)
      result = some(ctype)
    else:
      # All other meta types cannot be directly represented in C
      # They should be resolved before transpilation
      return none(CType)
  of TkStruct:
    var cstruct = CStructType(name: t.structType.name)
    for name, m in t.structType.members:
      let ctypeOpt = toCType(m.typ)
      if ctypeOpt.isNone:
        return none(CType)
      cstruct.members[name] = CStructMember(name: m.name, ctype: ctypeOpt.get())
    result = some(CType(kind: CkStruct, structType: cstruct))

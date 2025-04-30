import std/options
import std/tables
import ./types

type
  CTypeKind* = enum
    CkPrimitive
    CkStruct
    CkPointer
    CkConstPointer
    CkArray

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

  CArrayType* = object
    ctype*: CType
    size*: int

  CType* = ref object
    case kind*: CTypeKind
    of CkPrimitive:
      primitive*: CPrimitive
    of CkStruct:
      structType*: CStructType
    of CkPointer, CkConstPointer:
      ctype*: CType
    of CkArray:
      arrayType*: CArrayType

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
  of CkPointer, CkConstPointer:
    if a.ctype.isNil or b.ctype.isNil:
      result = a.ctype.isNil and b.ctype.isNil
    else:
      result = a.ctype == b.ctype
  of CkArray:
    if a.arrayType.size != b.arrayType.size:
      return false
    if a.arrayType.ctype.isNil or b.arrayType.ctype.isNil:
      result = a.arrayType.ctype.isNil and b.arrayType.ctype.isNil
    else:
      result = a.arrayType.ctype == b.arrayType.ctype

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
  of CkArray:
    if t.arrayType.ctype.isNil:
      result = "array(nil, " & $t.arrayType.size & ")"
    else:
      result = "array(" & $t.arrayType.ctype & ", " & $t.arrayType.size & ")"

proc getCString*(t: CType, varName: string = "", needParens: bool = false): string =
  ## Returns the C string representation of a CType, optionally with a variable name
  case t.kind
  of CkPrimitive:
    let baseType =
      case t.primitive
      of Int8T: "int8_t"
      of Int16T: "int16_t"
      of Int32T: "int32_t"
      of Int64T: "int64_t"
      of UInt8T: "uint8_t"
      of UInt16T: "uint16_t"
      of UInt32T: "uint32_t"
      of UInt64T: "uint64_t"
      of FloatT: "float"
      of DoubleT: "double"
      of BoolT: "_Bool"
      of CharT: "char"
      of VoidT: "void"
      of CharPtrT: "char*"
      of VarArgsT: "..."
    if varName.len > 0:
      result = baseType & " " & varName
    else:
      result = baseType
  of CkStruct:
    let baseType = "struct " & t.structType.name
    if varName.len > 0:
      result = baseType & " " & varName
    else:
      result = baseType
  of CkPointer:
    let subNeedsParens = t.ctype.kind in {CkArray, CkPointer, CkConstPointer}
    let innerVar =
      if varName.len > 0:
        if subNeedsParens:
          "(" & "*" & varName & ")"
        else:
          "*" & varName
      else:
        "*"
    result = getCString(t.ctype, innerVar)
  of CkConstPointer:
    let subNeedsParens = t.ctype.kind in {CkArray, CkPointer, CkConstPointer}
    let innerVar =
      if varName.len > 0:
        if subNeedsParens:
          "(" & "const*" & varName & ")"
        else:
          "const*" & varName
      else:
        "const*"
    result = getCString(t.ctype, innerVar)
  of CkArray:
    let innerVar =
      if varName.len > 0:
        varName & "[" & $t.arrayType.size & "]"
      else:
        "[" & $t.arrayType.size & "]"
    let needsParens =
      varName.len > 0 and (varName.contains('*') or varName.contains(')'))
    result = getCString(
      t.arrayType.ctype,
      if needsParens:
        "(" & innerVar & ")"
      else:
        innerVar,
    )

proc toCType*(t: Type, topLevel: bool = true): Option[CType] =
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
      let innerCTypeOpt = toCType(t.pointerTo, false)
      if innerCTypeOpt.isNone:
        return none(CType)
      innerCType = innerCTypeOpt.get()
      result = some(CType(kind: CkPointer, ctype: innerCType))
  of TkROPointer:
    if t.pointerTo.isNil:
      result = some(CType(kind: CkConstPointer, ctype: nil))
    else:
      var innerCType = new CType
      let innerCTypeOpt = toCType(t.pointerTo, false)
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
    if not topLevel:
      return some(CType(kind: CkStruct, structType: cstruct))
    for name, m in t.structType.members:
      let ctypeOpt = toCType(m.typ, false)
      if ctypeOpt.isNone:
        return none(CType)
      cstruct.members[name] = CStructMember(name: m.name, ctype: ctypeOpt.get())
    result = some(CType(kind: CkStruct, structType: cstruct))
  of TkArray:
    let elemCTypeOpt = toCType(t.arrayType.typ, false)
    if elemCTypeOpt.isNone:
      return none(CType)
    let elemCType = elemCTypeOpt.get()
    let arrType = CArrayType(ctype: elemCType, size: t.arrayType.size)
    result = some(CType(kind: CkArray, arrayType: arrType))

import std/options
import ./types

type

  CTypeKind* = enum
    CkPrimitive, CkStruct, CkUnion, CkEnum, CkTypedef, CkPointer

  CPrimitive* = enum
    Int8T, Int16T, Int32T, Int64T,
    UInt8T, UInt16T, UInt32T, UInt64T,
    FloatT, DoubleT,
    BoolT, CharT, VoidT, CharPtrT, VarArgsT
  
  CType* = object
    case kind*: CTypeKind
    of CkPrimitive:
      primitive*: CPrimitive
    of CkStruct, CkUnion, CkEnum, CkTypedef:
      discard # TODO
    of CkPointer:
      ctype*: ref CType

proc `==`*(a, b: CType): bool =
  ## Compares two CType objects for equality
  if a.kind != b.kind:
    return false
    
  case a.kind:
  of CkPrimitive:
    result = a.primitive == b.primitive
  of CkStruct, CkUnion, CkEnum, CkTypedef:
    result = true # To be implemented when these types are fully defined
  of CkPointer:
    if a.ctype.isNil or b.ctype.isNil:
      result = a.ctype.isNil and b.ctype.isNil
    else:
      result = a.ctype[] == b.ctype[]

proc `$`*(t: CType): string =
  ## Returns a string representation of a CType
  case t.kind:
  of CkPrimitive:
    result = $t.primitive
  of CkStruct:
    result = "struct" # To be expanded
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
      result = "pointer(" & $t.ctype[] & ")"

proc getCString*(t: CType): string =
  ## Returns the C string representation of a CType
  case t.kind:
  of CkPrimitive:
    case t.primitive:
    of Int8T: result = "int8_t"
    of Int16T: result = "int16_t"
    of Int32T: result = "int32_t"
    of Int64T: result = "int64_t"
    of UInt8T: result = "uint8_t"
    of UInt16T: result = "uint16_t"
    of UInt32T: result = "uint32_t"
    of UInt64T: result = "uint64_t"
    of FloatT: result = "float"
    of DoubleT: result = "double"
    of BoolT: result = "_Bool"
    of CharT: result = "char"
    of VoidT: result = "void"
    of CharPtrT: result = "char*"
    of VarArgsT: result = "..."
  of CkStruct:
    result = "struct" # To be expanded
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
      result = getCString(t.ctype[]) & "*"

proc toCType*(t: Type): Option[CType] =
  ## Converts a Type to a CType
  ## Returns none if the type cannot be represented in C
  case t.kind:
  of TkPrimitive:
    var ctype = CType(kind: CkPrimitive)
    case t.primitive:
    of Int, Int64: ctype.primitive = Int64T
    of Int8: ctype.primitive = Int8T
    of Int16: ctype.primitive = Int16T
    of Int32: ctype.primitive = Int32T
    of UInt, UInt64: ctype.primitive = UInt64T
    of UInt8: ctype.primitive = UInt8T
    of UInt16: ctype.primitive = UInt16T
    of UInt32: ctype.primitive = UInt32T
    of Float, Float64: ctype.primitive = DoubleT
    of Float32: ctype.primitive = FloatT
    of Bool: ctype.primitive = BoolT
    of Char: ctype.primitive = CharT
    of Void: ctype.primitive = VoidT
    of CString: ctype.primitive = CharPtrT
    of String: 
      return none(CType) # String type is not implemented yet
    result = some(ctype)
  of TkMeta:
    case t.metaKind:
    of MkCVarArgs:
      var ctype = CType(kind: CkPrimitive, primitive: VarArgsT)
      result = some(ctype)
    else:
      # All other meta types cannot be directly represented in C
      # They should be resolved before transpilation
      return none(CType)

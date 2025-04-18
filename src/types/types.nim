type
  TypeKind* = enum
    TkPrimitive
    TkPointer
    TkROPointer
    TkMeta

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
    MkUnresolved
    MkToInfer
    MkResolveError # Added for type resolution errors
    MkIntInfer
    MkFloatInfer
    MkUIntInfer

  Type* = object
    hasAddress*: bool # when type is from a variable
    case kind*: TypeKind
    of TkPrimitive:
      primitive*: Primitive
    of TkPointer, TkROPointer:
      pointerTo*: ref Type
    of TkMeta:
      case metaKind*: MetaKind
      of MkVarArgs, MkCVarArgs, MkToInfer, MkIntInfer, MkFloatInfer, MkUIntInfer:
        discard
      of MkUnresolved, MkResolveError:
        name*: string

proc newPointerType*(t: Type, isConst: bool): Type =
  ## Creates a new pointer type
  if isConst:
    result = Type(kind: TkROPointer, pointerTo: new Type)
  else:
    result = Type(kind: TkPointer, pointerTo: new Type)
  result.pointerTo[] = t

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
      result = a.pointerTo[] == b.pointerTo[]
  of TkMeta:
    if a.metaKind != b.metaKind:
      return false

    case a.metaKind
    of MkVarArgs, MkCVarArgs, MkToInfer, MkIntInfer, MkFloatInfer, MkUIntInfer:
      result = true # These are equal if the metaKind is the same (already checked)
    of MkUnresolved, MkResolveError:
      result = a.name == b.name

proc `$`*(t: Type): string =
  ## Returns a string representation of a Type
  case t.kind
  of TkPrimitive:
    result = $t.primitive
  of TkPointer:
    if t.pointerTo.isNil:
      result = "Pointer(nil)"
    else:
      result = "*" & $t.pointerTo[]
  of TkROPointer:
    if t.pointerTo.isNil:
      result = "ConstPointer(nil)"
    else:
      result = "ro*" & $t.pointerTo[]
  of TkMeta:
    case t.metaKind
    of MkVarArgs:
      result = "VarArgs"
    of MkCVarArgs:
      result = "CVarArgs"
    of MkToInfer:
      result = "_" # Placeholder for type inference
    of MkUnresolved:
      result = t.name
    of MkResolveError:
      result = "ResolveError(" & t.name & ")"
    of MkIntInfer:
      result = "SomeInt"
    of MkFloatInfer:
      result = "SomeFloat"
    of MkUIntInfer:
      result = "SomeUInt"

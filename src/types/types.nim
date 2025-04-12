type
  TypeKind* = enum
    TkPrimitive, TkMeta
  
  Primitive* = enum
    Int, Int8, Int16, Int32, Int64,
    UInt, UInt8, UInt16, UInt32, UInt64,
    Float, Float32, Float64,
    Bool, Char, String, Void, CString,
  
  MetaKind* = enum
    MkVarArgs, MkCVarArgs, 
    MkUnresolved, MkToInfer,
    MkResolveError, # Added for type resolution errors
    MkType,
    MkIntInfer, MkFloatInfer, MkUIntInfer

  Type* = object
    case kind*: TypeKind 
    of TkPrimitive:
      primitive*: Primitive
    of TkMeta:
      case metaKind*: MetaKind
      of MkVarArgs, MkCVarArgs, MkToInfer, MkIntInfer, MkFloatInfer, MkUIntInfer:
        discard
      of MkUnresolved, MkResolveError:
        name*: string
      of MkType:
        typeRepr*: ref Type

proc `==`*(a, b: Type): bool =
  ## Compares two Type objects for equality
  if a.kind != b.kind:
    return false
  
  case a.kind:
  of TkPrimitive:
    result = a.primitive == b.primitive
  of TkMeta:
    if a.metaKind != b.metaKind:
      return false
    
    case a.metaKind:
    of MkVarArgs, MkCVarArgs, MkToInfer, MkIntInfer, MkFloatInfer, MkUIntInfer:
      result = true # These are equal if the metaKind is the same (already checked)
    of MkUnresolved, MkResolveError:
      result = a.name == b.name
    of MkType:
      if a.typeRepr.isNil or b.typeRepr.isNil:
        result = a.typeRepr.isNil and b.typeRepr.isNil
      else:
        result = a.typeRepr[] == b.typeRepr[]

proc `$`*(t: Type): string =
  ## Returns a string representation of a Type
  case t.kind:
  of TkPrimitive:
    result = $t.primitive
  of TkMeta:
    case t.metaKind:
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
    of MkType:
      result = "Type(" & `$`(t.typeRepr[]) & ")"
    of MkIntInfer:
      result = "Int"
    of MkFloatInfer:
      result = "Float"
    of MkUIntInfer:
      result = "UInt"


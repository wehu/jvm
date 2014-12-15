{-# LANGUAGE FlexibleInstances, OverlappingInstances, RecordWildCards, NamedFieldPuns #-}

{-
   Copyright 2014 huwei04@hotmail.com
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
       http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}

module JavaClass (
    JavaClass(..),
    Version(..),
    ConstPool,
    ConstInfo(..),
    TypeSig(..),
    AccessFlag(..),
    FieldInfo(..),
    MethodInfo(..),
    AttrInfo(..),
    AttrData(..),
    CodeInfo(..),
    ExceptionInfo(..),
    InnerClassInfo(..),
    LineNumberInfo(..),
    LocalVariableInfo(..),
    Instrs,
    Instr(..),
    lookupConstString,
    lookupConstInteger,
    lookupConstLong,
    lookupConstFloat,
    lookupConstDouble,
    lookupClassName,
    lookupFieldName,
    lookupMethodName,
    lookupInteferceMethodName,
    lookupFieldInfo,
    lookupFieldAttrInfo,
    lookupMethodInfo,
    lookupMethodAttrInfo,
    lookupMethodCodeInfo,
    lookupMethodCodeMaxStack,
    lookupMethodCodeMaxLocals,
    lookupMethodCodeInstrs,
    lookupMethodExceptionInfo
    ) where

    import Data.List
    import Data.Array
    import qualified Data.Map as M
    import qualified Data.Set as S
    import Text.PrettyPrint

    class PP a where
        pp :: a -> Doc


    data JavaClass = JavaClass {
                                class_version      :: Version,
                                class_const_pool   :: ConstPool,
                                class_access_flags :: S.Set AccessFlag,
                                class_this         :: String,
                                class_super        :: String,
                                class_interfaces   :: S.Set String,
                                class_fields       :: M.Map (String, String) FieldInfo,
                                class_methods      :: M.Map (String, String) MethodInfo,
                                class_attributes   :: M.Map String AttrInfo
                               } deriving (Eq, Ord)


    instance Show JavaClass where
        show a = show $ pp a

    instance PP JavaClass where
        pp (JavaClass{..}) =
            vcat [
                  text "version       = " <+> text (show class_version),
                  text "constant pool = " <+> (pp class_const_pool),
                  text "access flags  = " <+> (pp $ S.toList class_access_flags),
                  text "this class    = " <+> text class_this,
                  text "super class   = " <+> text class_super,
                  text "interfacess   = " <+> (brackets $ vcat $ map text $ S.toList class_interfaces),
                  text "fields        = " <+> (pp $ M.elems class_fields),
                  text "methods       = " <+> (pp $ M.elems class_methods),
                  text "attributes    = " <+> (pp $ M.elems class_attributes)
                 ]

    data Version = Version Int Int deriving (Eq, Ord)

    instance Show Version where
    	show (Version minor major) = (show major) ++ "." ++ (show minor)

    type ConstPool = Array Int ConstInfo

    data ConstInfo =
        ConstNull
        | ConstUtf8 String
        | ConstInteger Int
        | ConstFloat Float
        | ConstLong Integer
        | ConstDouble Double
        | ConstClass Int
        | ConstString Int
        | ConstFieldRef Int Int
        | ConstMethodRef Int Int
        | ConstInterfaceMethodRef Int Int
        | ConstNameAndType Int Int deriving(Show, Eq, Ord)

    instance Show ConstPool where
        show l = show $ pp l

    instance PP ConstPool where
        pp l = let l' = elems l
                in brackets $ vcat $ map (\i -> (text $ show i)) $ zip [0..(length l')-1] l'

    data AccessFlag =
        AccPublic
        | AccPrivate
        | AccProtected
        | AccStatic
        | AccFinal
        | AccSynchronizedOrSuper
        | AccVolatile
        | AccTransient
        | AccSuper
        | AccInterface
        | AccAbstract
        | AccNative deriving (Show, Eq, Ord)

    instance Show [AccessFlag] where
        show l = show $ pp l

    instance PP [AccessFlag] where
        pp l = brackets $ vcat $ map (\i -> (text $ show i)) l

    data TypeSig =
        SigVoid
        | SigByte
        | SigChar
        | SigShort
        | SigInteger
        | SigLong
        | SigFloat
        | SigDouble
        | SigClass String
        | SigArray TypeSig
        | SigFunc [TypeSig] TypeSig deriving (Show, Eq, Ord)

    data FieldInfo = FieldInfo {
                                field_access_flags   :: S.Set AccessFlag,
                                field_name           :: String,
                                field_descriptor     :: String,
                                field_descriptor_sig :: TypeSig,
                                field_attributes     :: M.Map String AttrInfo
                               } deriving (Eq, Ord)

    instance Show FieldInfo where
        show a = show $ pp a

    instance PP FieldInfo where
        pp (FieldInfo{..}) =
            vcat [
                  text "access flags    = " <+> (pp $ S.toList field_access_flags),
                  text "name            = " <+> text field_name,
                  text "descriptor      = " <+> text field_descriptor,
                  text "descriptor sig  = " <+> (text $ show field_descriptor_sig),
                  text "attributes      = " <+> (pp $ M.elems field_attributes)
                 ]

    instance Show [FieldInfo] where
        show l = show $ pp l

    instance PP [FieldInfo] where
        pp l = brackets $ vcat $ intersperse comma $ map pp l

    data MethodInfo = MethodInfo {
                                  method_access_flags   :: S.Set AccessFlag,
                                  method_name           :: String,
                                  method_descriptor     :: String,
                                  method_descriptor_sig :: TypeSig,
                                  method_attributes     :: M.Map String AttrInfo
                                 } deriving (Eq, Ord)

    instance Show MethodInfo where
        show a = show $ pp a

    instance PP MethodInfo where
        pp (MethodInfo{..}) =
            vcat [
                  text "access flags   = " <+> (pp $ S.toList method_access_flags),
                  text "name           = " <+> text method_name,
                  text "descriptor     = " <+> text method_descriptor,
                  text "descriptor sig = " <+> (text $ show method_descriptor_sig),
                  text "attributes     = " <+> (pp $ M.elems method_attributes)
                 ]

    instance Show [MethodInfo] where
        show l = show $ pp l

    instance PP [MethodInfo] where
        pp l = brackets $ vcat $ intersperse comma $ map pp l

    data AttrInfo = AttrInfo {
                              attribute_name :: String,
                              attribute_info :: AttrData
                             } deriving (Eq, Ord)

    instance Show AttrInfo where
        show a = show $ pp a

    instance PP AttrInfo where
        pp (AttrInfo{..}) = 
            vcat [
                  text "name = " <+> text attribute_name,
                  text "info = " <+> (pp attribute_info)
                 ]

    instance Show [AttrInfo] where
        show l = show $ pp l

    instance PP [AttrInfo] where
        pp l = brackets $ vcat $ intersperse comma $ map pp l

    data AttrData = 
        Code CodeInfo
        | ConstValue Int
        | Exceptions [String]
        | InnerClasses [InnerClassInfo]
        | LineNumbers [LineNumberInfo]
        | LocalVariables [LocalVariableInfo]
        | SourceFile String
        | Synthetic deriving (Eq, Ord)

    instance Show AttrData where
        show a = show $ pp a

    instance PP AttrData where
        pp (Code ci) = pp ci
        pp (ConstValue i) = text $ show i
        pp (Exceptions es) = brackets $ vcat $ map text es
        pp (InnerClasses cl) = brackets $ vcat $ intersperse comma $ map pp cl
        pp (LineNumbers ll) = brackets $ vcat $ intersperse comma $ map pp ll
        pp (LocalVariables ll) = brackets $ vcat $ intersperse comma $ map pp ll
        pp (SourceFile sf) = text sf
        pp Synthetic = text "Synthetic"

    data CodeInfo = CodeInfo {
                              code_max_stack  :: Int,
                              code_max_locals :: Int,
                              code_instrs     :: Instrs,
                              code_exceptions :: [ExceptionInfo],
                              code_attributes :: M.Map String AttrInfo
                             } deriving (Eq, Ord)

    instance Show CodeInfo where
        show a = show $ pp a

    instance PP CodeInfo where
        pp (CodeInfo ms ml cs ces cas) =
            vcat [
                  text "max stack  = " <+> text (show ms),
                  text "max locals = " <+> text (show ml),
                  text "codes      = " <+> (brackets $ vcat $ map (\i -> text (show i)) $ elems cs),
                  text "exceptions = " <+> (brackets $ vcat $ intersperse comma $ map pp ces),
                  text "attributes = " <+> (pp $ M.elems cas)
                 ]

    data ExceptionInfo = ExceptionInfo {
                                        exception_start_pc   :: Int,
                                        exception_end_pc     :: Int,
                                        exception_handler_pc :: Int,
                                        exception_catch_type :: Int
                                       } deriving (Eq, Ord)

    instance Show ExceptionInfo where
        show a = show $ pp a

    instance PP ExceptionInfo where
        pp (ExceptionInfo{..}) =
            vcat [
                  text "start pc   = " <+> text (show exception_start_pc),
                  text "end pc     = " <+> text (show exception_end_pc),
                  text "handler pc = " <+> text (show exception_handler_pc),
                  text "catch type = " <+> text (show exception_catch_type)
                 ]

    data InnerClassInfo = InnerClassInfo {
                                          inner_class_name         :: String,
                                          outer_class_name         :: String,
                                          inner_name               :: String,
                                          inner_class_access_flags :: S.Set AccessFlag
                                         } deriving (Eq, Ord)

    instance Show InnerClassInfo where
        show a = show $ pp a

    instance PP InnerClassInfo where
        pp (InnerClassInfo{..}) =
            vcat [
                  text "inner class name  = " <+> text inner_class_name,
                  text "outer class name  = " <+> text outer_class_name,
                  text "class name        = " <+> text inner_name,
                  text "access flags      = " <+> (pp $ S.toList inner_class_access_flags)
                 ]

    data LineNumberInfo = LineNumberInfo {
                                          line_number_start_pc :: Int,
                                          line_number          :: Int
                                         } deriving (Eq, Ord)

    instance Show LineNumberInfo where
        show a = show $ pp a

    instance PP LineNumberInfo where
        pp (LineNumberInfo{..}) =
            vcat [
                  text "start pc    = " <+> text (show line_number_start_pc),
                  text "line number = " <+> text (show line_number)
                 ]

    data LocalVariableInfo = LocalVariableInfo {
                                                local_variable_start_pc   :: Int,
                                                local_variable_length     :: Int,
                                                local_variable_name       :: String,
                                                local_variable_descriptor :: String,
                                                local_variable_index      :: Int
                                               } deriving (Eq, Ord)

    instance Show LocalVariableInfo where
        show a = show $ pp a

    instance PP LocalVariableInfo where
        pp (LocalVariableInfo{..}) =
            vcat [
                  text "start pc   = " <+> text (show local_variable_start_pc),
                  text "length     = " <+> text (show local_variable_length),
                  text "name       = " <+> text local_variable_name,
                  text "descriptor = " <+> text local_variable_descriptor,
                  text "index      = " <+> text (show local_variable_index)
                 ]

    type Instrs = Array Int Instr

    data Instr = 
        I_unknown
        | I_nop
        | I_aconst_null
        | I_iconst_m1
        | I_iconst_0
        | I_iconst_1
        | I_iconst_2
        | I_iconst_3
        | I_iconst_4
        | I_iconst_5
        | I_lconst_0
        | I_lconst_1
        | I_fconst_0
        | I_fconst_1
        | I_fconst_2
        | I_dconst_0
        | I_dconst_1
        | I_bipush Int
        | I_sipush Int
        | I_ldc Int
        | I_ldc_w Int
        | I_ldc2_w Int
        | I_iload Int
        | I_lload Int
        | I_fload Int
        | I_dload Int
        | I_aload Int
        | I_iload_0
        | I_iload_1
        | I_iload_2
        | I_iload_3
        | I_lload_0
        | I_lload_1
        | I_lload_2
        | I_lload_3
        | I_fload_0
        | I_fload_1
        | I_fload_2
        | I_fload_3
        | I_dload_0
        | I_dload_1
        | I_dload_2
        | I_dload_3
        | I_aload_0
        | I_aload_1
        | I_aload_2
        | I_aload_3
        | I_iaload
        | I_laload
        | I_faload
        | I_daload
        | I_aaload
        | I_baload
        | I_caload
        | I_saload
        | I_istore Int
        | I_lstore Int
        | I_fstore Int
        | I_dstore Int
        | I_astore Int
        | I_istore_0
        | I_istore_1
        | I_istore_2
        | I_istore_3
        | I_lstore_0
        | I_lstore_1
        | I_lstore_2
        | I_lstore_3
        | I_fstore_0
        | I_fstore_1
        | I_fstore_2
        | I_fstore_3
        | I_dstore_0
        | I_dstore_1
        | I_dstore_2
        | I_dstore_3
        | I_astore_0
        | I_astore_1
        | I_astore_2
        | I_astore_3
        | I_iastore
        | I_lastore
        | I_fastore
        | I_dastore
        | I_aastore
        | I_bastore
        | I_castore
        | I_sastore
        | I_pop
        | I_pop2
        | I_dup
        | I_dup_x1
        | I_dup_x2
        | I_dup2
        | I_dup2_x1
        | I_dup2_x2
        | I_swap
        | I_iadd
        | I_ladd
        | I_fadd
        | I_dadd
        | I_isub
        | I_lsub
        | I_fsub
        | I_dsub
        | I_imul
        | I_lmul
        | I_fmul
        | I_dmul
        | I_idiv
        | I_ldiv
        | I_fdiv
        | I_ddiv
        | I_irem
        | I_lrem
        | I_frem
        | I_drem
        | I_ineg
        | I_lneg
        | I_fneg
        | I_dneg
        | I_ishl
        | I_lshl
        | I_ishr
        | I_lshr
        | I_iushr
        | I_lushr
        | I_iand
        | I_land
        | I_ior
        | I_lor
        | I_ixor
        | I_lxor
        | I_iinc Int Int
        | I_i2l
        | I_i2f
        | I_i2d
        | I_l2i
        | I_l2f
        | I_l2d
        | I_f2i
        | I_f2l
        | I_f2d
        | I_d2i
        | I_d2l
        | I_d2f
        | I_i2b
        | I_i2c
        | I_i2s
        | I_lcmp
        | I_fcmpl
        | I_fcmpg
        | I_dcmpl
        | I_dcmpg
        | I_ifeq Int
        | I_ifne Int
        | I_iflt Int
        | I_ifge Int
        | I_ifgt Int
        | I_ifle Int
        | I_if_icmpeq Int
        | I_if_icmpne Int
        | I_if_icmplt Int
        | I_if_icmpge Int
        | I_if_icmpgt Int
        | I_if_icmple Int
        | I_if_acmpeq Int
        | I_if_acmpne Int
        | I_goto Int
        | I_jsr Int
        | I_ret Int
        | I_tableswitch Int Int Int [Int]
        | I_lookupswitch Int (M.Map Int Int)
        | I_ireturn
        | I_lreturn
        | I_freturn
        | I_dreturn
        | I_areturn
        | I_return
        | I_getstatic Int
        | I_putstatic Int
        | I_getfield Int
        | I_putfield Int
        | I_invokevirtual Int
        | I_invokespecial Int
        | I_invokestatic Int
        | I_invokeinterface Int Int
        | I_new Int
        | I_newarray Int
        | I_anewarray Int
        | I_arraylength
        | I_athrow
        | I_checkcast Int
        | I_instanceof Int
        | I_monitorenter
        | I_monitorexit
        | I_wide
        | I_multianewarray Int Int
        | I_ifnull Int
        | I_ifnonnull Int
        | I_goto_w Int
        | I_jsr_w Int
        | I_ldc_quick Int
        | I_ldc_w_quick Int
        | I_ldc2_w_quick Int
        | I_getfield_quick Int
        | I_putfield_quick Int
        | I_getfield2_quick Int
        | I_putfield2_quick Int
        | I_getstatic_quick Int
        | I_putstatic_quick Int
        | I_getstatic2_quick Int
        | I_putstatic2_quick Int
        | I_invokevirtual_quick Int
        | I_invokenonvirtual_quick Int
        | I_invokesuper_quick Int
        | I_invokestatic_quick Int
        | I_invokeinterface_quick Int Int
        | I_invokevirtualobject_quick Int
        | I_new_quick Int
        | I_anewarray_quick Int
        | I_multianewarray_quick Int Int
        | I_checkcast_quick Int
        | I_instanceof_quick Int
        | I_invokevirtual_quick_w Int
        | I_getfield_quick_w Int
        | I_putfield_quick_w Int
        | I_breakpoint
        | I_impdep1
        | I_impdep2
        | I_iload_w Int
        | I_lload_w Int
        | I_fload_w Int
        | I_dload_w Int
        | I_aload_w Int
        | I_istore_w Int
        | I_lstore_w Int
        | I_fstore_w Int
        | I_dstore_w Int
        | I_astore_w Int
        | I_iinc_w Int Int
        | I_ret_w Int
        deriving (Show, Eq, Ord)

    lookupConstString :: Int -> JavaClass -> String
    lookupConstString i c =
        let cp = class_const_pool c
            ConstUtf8 s = cp ! i
         in s

    lookupConstInteger :: Int -> JavaClass -> Int
    lookupConstInteger i c =
        let cp = class_const_pool c
            ConstInteger v = cp ! i
         in v

    lookupConstLong :: Int -> JavaClass -> Integer
    lookupConstLong i c =
        let cp = class_const_pool c
            ConstLong v = cp ! i
         in v

    lookupConstFloat :: Int -> JavaClass -> Float
    lookupConstFloat i c =
        let cp = class_const_pool c
            ConstFloat v = cp ! i
         in v

    lookupConstDouble :: Int -> JavaClass -> Double
    lookupConstDouble i c =
        let cp = class_const_pool c
            ConstDouble v = cp ! i
         in v

    lookupClassName :: Int -> JavaClass -> String
    lookupClassName i c =
        let cp = class_const_pool c
            ConstClass i' = cp ! i
            ConstUtf8 n = cp ! i'
         in n

    lookupFieldName :: Int -> JavaClass -> (String, String, String)
    lookupFieldName i c =
        let cp = class_const_pool c
            ConstFieldRef ci fi = cp ! i
            ConstClass cc = cp ! ci
            ConstNameAndType ni ti = cp ! fi
            ConstUtf8 cn = cp ! cc
            ConstUtf8 n = cp ! ni
            ConstUtf8 t = cp ! ti
         in (cn, n, t)

    lookupMethodName :: Int -> JavaClass -> (String, String, String)
    lookupMethodName i c =
        let cp = class_const_pool c
            ConstMethodRef ci mi = cp ! i
            ConstClass cc = cp ! ci
            ConstNameAndType ni ti = cp ! mi
            ConstUtf8 cn = cp ! cc
            ConstUtf8 n = cp ! ni
            ConstUtf8 t = cp ! ti
         in (cn, n, t)

    lookupInteferceMethodName :: Int -> JavaClass -> (String, String, String)
    lookupInteferceMethodName i c =
        let cp = class_const_pool c
            ConstInterfaceMethodRef ci mi = cp ! i
            ConstClass cc = cp ! ci
            ConstNameAndType ni ti = cp ! mi
            ConstUtf8 cn = cp ! cc
            ConstUtf8 n = cp ! ni
            ConstUtf8 t = cp ! ti
         in (cn, n, t)

    lookupFieldInfo :: (String, String) -> JavaClass -> Maybe FieldInfo
    lookupFieldInfo n c = M.lookup n $ class_fields c

    lookupFieldAttrInfo :: (String, String) -> String -> JavaClass -> Maybe AttrInfo
    lookupFieldAttrInfo n a c =
        case lookupFieldInfo n c of
            Nothing -> Nothing
            Just fi -> (M.lookup a $ field_attributes fi)

    lookupMethodInfo :: (String, String) -> JavaClass -> Maybe MethodInfo
    lookupMethodInfo n c = M.lookup n $ class_methods c

    lookupMethodAttrInfo :: (String, String) -> String -> JavaClass -> Maybe AttrInfo
    lookupMethodAttrInfo n a c =
        case lookupMethodInfo n c of
            Nothing -> Nothing
            Just mi -> (M.lookup a $ method_attributes mi)

    lookupMethodCodeInfo :: (String, String) -> JavaClass -> Maybe CodeInfo
    lookupMethodCodeInfo n c =
        case lookupMethodAttrInfo n "Code" c of
            Nothing -> Nothing
            Just a -> let (Code cs) = attribute_info a
                       in Just cs

    lookupMethodCodeMaxStack :: (String, String) -> JavaClass -> Maybe Int
    lookupMethodCodeMaxStack n c =
        case lookupMethodCodeInfo n c of
            Nothing -> Nothing
            Just ci -> Just $ code_max_stack ci

    lookupMethodCodeMaxLocals :: (String, String) -> JavaClass -> Maybe Int
    lookupMethodCodeMaxLocals n c =
        case lookupMethodCodeInfo n c of
            Nothing -> Nothing
            Just ci -> Just $ code_max_locals ci

    lookupMethodCodeInstrs :: (String, String) -> JavaClass -> Maybe Instrs
    lookupMethodCodeInstrs n c =
        case lookupMethodCodeInfo n c of
            Nothing -> Nothing
            Just ci -> Just $ code_instrs ci

    lookupMethodExceptionInfo :: (String, String) -> JavaClass -> Maybe [ExceptionInfo]
    lookupMethodExceptionInfo n c =
        case lookupMethodCodeInfo n c of
            Nothing -> Nothing
            Just ci -> Just $ code_exceptions ci
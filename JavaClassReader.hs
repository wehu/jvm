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

module JavaClassReader(
    readTypeSig,
    loadClass
    ) where

    import Text.ParserCombinators.Parsec
    import Data.Char
    import Data.Bits
    import Data.Binary.IEEE754
    import Data.Word
    import Data.List
    import Data.Array
    import Control.Applicative hiding (many, (<|>))
    import qualified Data.Map as M
    import qualified Data.Set as S
    import qualified Data.ByteString.Char8 as BS (readFile, unpack)

    import JavaClass

    -- | Java class parser type
    --
    type JCParser a = CharParser ConstPool a

    -- | Read 1 byte
    --
    readu1 :: JCParser Int
    readu1 = ord <$> anyChar

    -- | Read 2 bytes
    --
    readu2 :: JCParser Int
    readu2 = (\a b -> (shift a 8)  + b) <$> readu1 <*> readu1

    -- | Read 4 bytes
    --
    readu4 :: JCParser Int
    readu4 = (\a b -> (shift a 16) + b) <$> readu2 <*> readu2

    -- | Read 8 bytes
    --
    readu8 :: JCParser Integer
    readu8 = (\a b -> ((shift $ toInteger a) 32) + (toInteger b)) <$> readu4 <*> readu4

    -- | Read a utf8 string
    --
    readutf8 :: JCParser String
    readutf8 = do
        l <- readu2
        count l anyChar

    -- | Read a float
    --
    readfloat :: JCParser Float
    readfloat = (\f -> wordToFloat $ fromInteger $ toInteger f) <$> readu4

    -- | Read a double
    --
    readdouble :: JCParser Double
    readdouble = (\d -> wordToDouble $ fromInteger $ toInteger d) <$> readu8

    -- | Read a java class
    --
    readClass :: JCParser JavaClass
    readClass = do
        -- magic code : CAFEBABE
        string "\202\254\186\190"
        -- read version
        v   <- readVersion
        -- read constant pool
        cp  <- readConstPool
        -- set constant pool
        setState cp
        -- read access flags
        afs <- readAccessFlags
        -- read this class name
        tc  <- readClassName False
        -- read super class name
        sc  <- readClassName True
        -- read interface list
        is  <- readInterfaces
        -- read fields
        fs  <- readFields
        -- read methods
        ms  <- readMethods
        -- read attributes
        as  <- readAttrs
        eof
        return $ JavaClass {
                            class_version      = v,
                            class_const_pool   = cp,
                            class_access_flags = afs,
                            class_this         = tc,
                            class_super        = sc,
                            class_interfaces   = is,
                            class_fields       = fs,
                            class_methods      = ms,
                            class_attributes   = as
                           }

    -- | Read version
    -- 
    readVersion :: JCParser Version
    readVersion = Version <$> readu2 <*> readu2

    -- | Read constant pool
    --
    readConstPool :: JCParser ConstPool
    readConstPool = do
        l  <- readu2
        cp <- count (l-1) readConstInfo
        let cp' = (ConstNull:cp)
        return $ listArray (0, (length cp') - 1) cp'

    readConstInfo :: JCParser ConstInfo
    readConstInfo = do
        tag <- readu1
        case tag of
            1  -> ConstUtf8               <$> readutf8
            3  -> ConstInteger            <$> readu4
            4  -> ConstFloat              <$> readfloat
            5  -> ConstLong               <$> readu8
            6  -> ConstDouble             <$> readdouble
            7  -> ConstClass              <$> readu2
            8  -> ConstString             <$> readu2
            9  -> ConstFieldRef           <$> readu2 <*> readu2
            10 -> ConstMethodRef          <$> readu2 <*> readu2
            11 -> ConstInterfaceMethodRef <$> readu2 <*> readu2
            12 -> ConstNameAndType        <$> readu2 <*> readu2
            _ -> error $ "unknown constant tag " ++ (show tag)

    readAccessFlags :: JCParser (S.Set AccessFlag)
    readAccessFlags = do
        fs <- readu2
        return $ S.fromList $ foldl'
          (\acc (a, n) ->
              if (fs .&. n) == 0 then acc else (a:acc))
          []
          [(AccPublic, 0x0001),
           (AccPrivate, 0x0002),
           (AccProtected, 0x0004),
           (AccStatic, 0x0008),
           (AccFinal, 0x0010),
           (AccSynchronizedOrSuper, 0x0020),
           (AccVolatile, 0x0040),
           (AccTransient, 0x0080),
           (AccNative, 0x0100),
           (AccInterface, 0x0200),
           (AccAbstract, 0x0400)]

    readName :: Bool -> JCParser String
    readName relex = do
        i  <- readu2
        cp <- getState
        if i == 0 && relex
        then return ""
        else let ConstUtf8 s = cp ! i
              in return s

    readDescriptor :: JCParser String
    readDescriptor = readName False

    readClassName :: Bool -> JCParser String
    readClassName relex = do
        ci <- readu2
        if ci == 0 && relex
        then return ""
        else do
            cp <- getState
            let ConstClass si = cp ! ci
                ConstUtf8  s  = cp ! si
             in return s

    readInterfaces :: JCParser (S.Set String)
    readInterfaces = do
        l  <- readu2
        is <- count l $ readClassName False
        return $ S.fromList is

    readFields :: JCParser (M.Map (String, String) FieldInfo)
    readFields = do
        l  <- readu2
        fs <- count l readField
        return $ M.fromList $ map (\f -> ((field_name f, field_descriptor f), f)) fs

    readField :: JCParser FieldInfo
    readField = do
        afs <- readAccessFlags
        fn  <- readName False
        d   <- readDescriptor
        as  <- readAttrs
        case readTypeSig d of
            Left err -> error err
            Right s -> return $ FieldInfo {
                                           field_access_flags   = afs,
                                           field_name           = fn,
                                           field_descriptor     = d,
                                           field_descriptor_sig = s,
                                           field_attributes     = as
                                          }

    readAttrs :: JCParser (M.Map String AttrInfo)
    readAttrs = do
        l  <- readu2
        as <- count l readAttr
        return $ M.fromList $ map (\a -> (attribute_name a, a)) as

    readAttr :: JCParser AttrInfo
    readAttr = do
        n <- readName False
        l <- readu4
        d <- case n of
            "Code"               -> readCode
            "ConstantValue"      -> readConstantValue
            "Exceptions"         -> readExceptions
            "InnerClasses"       -> readInnerClasses
            "LineNumberTable"    -> readLineNumberTable
            "LocalVariableTable" -> readLocalVariableTable
            "SourceFile"         -> readSourceFile
            "Synthetic"          -> readSynthetic
            "StackMapTable"      -> readStackMapTable
            _                    -> error $ "unknown attribute type " ++ n
        return $ AttrInfo {
                           attribute_name = n,
                           attribute_info = d
                          }

    readPaddings :: JCParser Int
    readPaddings = do
        ps <- many $ char '\000'
        return $ length ps

    readCode :: JCParser AttrData
    readCode = do
        ci <- CodeInfo <$>
                  readu2 <*>
                  readu2 <*>
                  readInstrs <*>
                  readExceptionTable <*>
                  readAttrs
        return $ Code ci

    readInstrs :: JCParser Instrs
    readInstrs = do
        l  <- readu4
        cs <- readInstr l
        return $ listArray (0, l-1) cs

    generateUnknownInstr :: Int -> [Instr]
    generateUnknownInstr i = replicate i I_unknown

    readInstr0 :: Instr -> JCParser [Instr]
    readInstr0 i = return [i]

    readInstr1 :: (Int -> Instr) -> JCParser [Instr]
    readInstr1 g = (\index -> [g index, I_unknown]) <$> readu1

    readInstr2 :: (Int -> Instr) -> JCParser [Instr]
    readInstr2 g = (\index -> [g index, I_unknown, I_unknown]) <$> readu2

    readInstr2w :: (Int -> Instr) -> JCParser [Instr]
    readInstr2w g = (\index -> [g index, I_unknown, I_unknown, I_unknown]) <$> readu2

    readInstr4 :: (Int -> Instr) -> JCParser [Instr]
    readInstr4 g = 
        (\index -> [g index, I_unknown, I_unknown, I_unknown, I_unknown]) <$> readu4

    readInstr :: Int -> JCParser [Instr]
    readInstr i =
        if i == 0
        then return []
        else do
            c     <- readu1
            instr <- case c of
                         0   -> readInstr0 I_nop
                         1   -> readInstr0 I_aconst_null
                         2   -> readInstr0 I_iconst_m1
                         3   -> readInstr0 I_iconst_0
                         4   -> readInstr0 I_iconst_1
                         5   -> readInstr0 I_iconst_2
                         6   -> readInstr0 I_iconst_3
                         7   -> readInstr0 I_iconst_4
                         8   -> readInstr0 I_iconst_5
                         9   -> readInstr0 I_lconst_0
                         10  -> readInstr0 I_lconst_1
                         11  -> readInstr0 I_fconst_0
                         12  -> readInstr0 I_fconst_1
                         13  -> readInstr0 I_fconst_2
                         14  -> readInstr0 I_dconst_0
                         15  -> readInstr0 I_dconst_1
                         16  -> readInstr1 I_bipush
                         17  -> readInstr2 I_sipush
                         18  -> readInstr1 I_ldc
                         19  -> readInstr2 I_ldc_w
                         20  -> readInstr2 I_ldc2_w
                         21  -> readInstr1 I_iload
                         22  -> readInstr1 I_lload
                         23  -> readInstr1 I_fload
                         24  -> readInstr1 I_dload
                         25  -> readInstr1 I_aload
                         26  -> readInstr0 I_iload_0
                         27  -> readInstr0 I_iload_1
                         28  -> readInstr0 I_iload_2
                         29  -> readInstr0 I_iload_3
                         30  -> readInstr0 I_lload_0
                         31  -> readInstr0 I_lload_1
                         32  -> readInstr0 I_lload_2
                         33  -> readInstr0 I_lload_3
                         34  -> readInstr0 I_fload_0
                         35  -> readInstr0 I_fload_1
                         36  -> readInstr0 I_fload_2
                         37  -> readInstr0 I_fload_3
                         38  -> readInstr0 I_dload_0
                         39  -> readInstr0 I_dload_1
                         40  -> readInstr0 I_dload_2
                         41  -> readInstr0 I_dload_3
                         42  -> readInstr0 I_aload_0
                         43  -> readInstr0 I_aload_1
                         44  -> readInstr0 I_aload_2
                         45  -> readInstr0 I_aload_3
                         46  -> readInstr0 I_iaload
                         47  -> readInstr0 I_laload
                         48  -> readInstr0 I_faload
                         49  -> readInstr0 I_daload
                         50  -> readInstr0 I_aaload
                         51  -> readInstr0 I_baload
                         52  -> readInstr0 I_caload
                         53  -> readInstr0 I_saload
                         54  -> readInstr1 I_istore
                         55  -> readInstr1 I_lstore
                         56  -> readInstr1 I_fstore
                         57  -> readInstr1 I_dstore
                         58  -> readInstr1 I_astore
                         59  -> readInstr0 I_istore_0
                         60  -> readInstr0 I_istore_1
                         61  -> readInstr0 I_istore_2
                         62  -> readInstr0 I_istore_3
                         63  -> readInstr0 I_lstore_0
                         64  -> readInstr0 I_lstore_1
                         65  -> readInstr0 I_lstore_2
                         66  -> readInstr0 I_lstore_3
                         67  -> readInstr0 I_fstore_0
                         68  -> readInstr0 I_fstore_1
                         69  -> readInstr0 I_fstore_2
                         70  -> readInstr0 I_fstore_3
                         71  -> readInstr0 I_dstore_0
                         72  -> readInstr0 I_dstore_1
                         73  -> readInstr0 I_dstore_2
                         74  -> readInstr0 I_dstore_3
                         75  -> readInstr0 I_astore_0
                         76  -> readInstr0 I_astore_1
                         77  -> readInstr0 I_astore_2
                         78  -> readInstr0 I_astore_3
                         79  -> readInstr0 I_iastore
                         80  -> readInstr0 I_lastore
                         81  -> readInstr0 I_fastore
                         82  -> readInstr0 I_dastore
                         83  -> readInstr0 I_aastore
                         84  -> readInstr0 I_bastore
                         85  -> readInstr0 I_castore
                         86  -> readInstr0 I_sastore
                         87  -> readInstr0 I_pop
                         88  -> readInstr0 I_pop2
                         89  -> readInstr0 I_dup
                         90  -> readInstr0 I_dup_x1
                         91  -> readInstr0 I_dup_x2
                         92  -> readInstr0 I_dup2
                         93  -> readInstr0 I_dup2_x1
                         94  -> readInstr0 I_dup2_x2
                         95  -> readInstr0 I_swap
                         96  -> readInstr0 I_iadd
                         97  -> readInstr0 I_ladd
                         98  -> readInstr0 I_fadd
                         99  -> readInstr0 I_dadd
                         100 -> readInstr0 I_isub
                         101 -> readInstr0 I_lsub
                         102 -> readInstr0 I_fsub
                         103 -> readInstr0 I_dsub
                         104 -> readInstr0 I_imul
                         105 -> readInstr0 I_lmul
                         106 -> readInstr0 I_fmul
                         107 -> readInstr0 I_dmul
                         108 -> readInstr0 I_idiv
                         109 -> readInstr0 I_ldiv
                         110 -> readInstr0 I_fdiv
                         111 -> readInstr0 I_ddiv
                         112 -> readInstr0 I_irem
                         113 -> readInstr0 I_lrem
                         114 -> readInstr0 I_frem
                         115 -> readInstr0 I_drem
                         116 -> readInstr0 I_ineg
                         117 -> readInstr0 I_lneg
                         118 -> readInstr0 I_fneg
                         119 -> readInstr0 I_dneg
                         120 -> readInstr0 I_ishl
                         121 -> readInstr0 I_lshl
                         122 -> readInstr0 I_ishr
                         123 -> readInstr0 I_lshr
                         124 -> readInstr0 I_iushr
                         125 -> readInstr0 I_lushr
                         126 -> readInstr0 I_iand
                         127 -> readInstr0 I_land
                         128 -> readInstr0 I_ior
                         129 -> readInstr0 I_lor
                         130 -> readInstr0 I_ixor
                         131 -> readInstr0 I_lxor
                         132 -> do
                             index <- readu1
                             c     <- readu1
                             return $ [I_iinc index c] ++ (generateUnknownInstr 2)
                         133 -> readInstr0 I_i2l
                         134 -> readInstr0 I_i2f
                         135 -> readInstr0 I_i2d
                         136 -> readInstr0 I_l2i
                         137 -> readInstr0 I_l2f
                         138 -> readInstr0 I_l2d
                         139 -> readInstr0 I_f2i
                         140 -> readInstr0 I_f2l
                         141 -> readInstr0 I_f2d
                         142 -> readInstr0 I_d2i
                         143 -> readInstr0 I_d2l
                         144 -> readInstr0 I_d2f
                         145 -> readInstr0 I_i2b
                         146 -> readInstr0 I_i2c
                         147 -> readInstr0 I_i2s
                         148 -> readInstr0 I_lcmp
                         149 -> readInstr0 I_fcmpl
                         150 -> readInstr0 I_fcmpg
                         151 -> readInstr0 I_dcmpl
                         152 -> readInstr0 I_dcmpg
                         153 -> readInstr2 I_ifeq
                         154 -> readInstr2 I_ifne
                         155 -> readInstr2 I_iflt
                         156 -> readInstr2 I_ifge
                         157 -> readInstr2 I_ifgt
                         158 -> readInstr2 I_ifle
                         159 -> readInstr2 I_if_icmpeq
                         160 -> readInstr2 I_if_icmpne
                         161 -> readInstr2 I_if_icmplt
                         162 -> readInstr2 I_if_icmpge
                         163 -> readInstr2 I_if_icmpgt
                         164 -> readInstr2 I_if_icmple
                         165 -> readInstr2 I_if_acmpeq
                         166 -> readInstr2 I_if_acmpne
                         167 -> readInstr2 I_goto
                         168 -> readInstr2 I_jsr
                         169 -> readInstr1 I_ret
                         170 -> do
                            ps <- readPaddings
                            d  <- readu4
                            l  <- readu4
                            h  <- readu4
                            os <- count (h-l+1) readu4
                            return $ [I_tableswitch d l h os] ++ (generateUnknownInstr $ ps + 12 + (length os) * 4)
                         171 -> do
                            ps <- readPaddings
                            d  <- readu4
                            n  <- readu4
                            ms <- let p = do
                                            m <- readu4
                                            o <- readu4
                                            return (m, o)
                                    in count n p
                            return $ [I_lookupswitch d (M.fromList ms)] ++ (generateUnknownInstr $ ps + 8 + (length ms) * 8)
                         172 -> readInstr0 I_ireturn
                         173 -> readInstr0 I_lreturn
                         174 -> readInstr0 I_freturn
                         175 -> readInstr0 I_dreturn
                         176 -> readInstr0 I_areturn
                         177 -> readInstr0 I_return
                         178 -> readInstr2 I_getstatic
                         179 -> readInstr2 I_putstatic
                         180 -> readInstr2 I_getfield
                         181 -> readInstr2 I_putfield
                         182 -> readInstr2 I_invokevirtual
                         183 -> readInstr2 I_invokespecial
                         184 -> readInstr2 I_invokestatic
                         185 -> do
                             index <- readu2
                             nargs <- readu1
                             char '\000'
                             return $ [I_invokeinterface index nargs] ++ (generateUnknownInstr 4)
                         187 -> readInstr2 I_new
                         188 -> readInstr1 I_newarray
                         189 -> readInstr2 I_anewarray
                         190 -> readInstr0 I_arraylength
                         191 -> readInstr0 I_athrow
                         192 -> readInstr2 I_checkcast
                         193 -> readInstr2 I_instanceof
                         194 -> readInstr0 I_monitorenter
                         195 -> readInstr0 I_monitorexit
                         196 -> do
                             op <- readu1
                             case op of
                                21 -> readInstr2w I_iload_w
                                22 -> readInstr2w I_lload_w
                                23 -> readInstr2w I_fload_w
                                24 -> readInstr2w I_dload_w
                                25 -> readInstr2w I_aload_w
                                54 -> readInstr2w I_istore_w
                                55 -> readInstr2w I_lstore_w
                                56 -> readInstr2w I_fstore_w
                                57 -> readInstr2w I_dstore_w
                                58 -> readInstr2w I_astore_w
                                132 -> do
                                    index <- readu2
                                    c <- readu2
                                    return $ [I_iinc_w index c] ++ (generateUnknownInstr 5)
                                169 -> readInstr2w I_ret_w
                                _ -> error $ "unknown opcode for wide " ++ (show op)
                         197 -> do
                             index <- readu2
                             d <- readu1
                             return $ [I_multianewarray index d] ++ (generateUnknownInstr 3)
                         198 -> readInstr2 I_ifnull
                         199 -> readInstr2 I_ifnonnull
                         200 -> readInstr4 I_goto_w
                         201 -> readInstr4 I_jsr_w
                         202 -> readInstr0 I_breakpoint
                         254 -> readInstr0 I_impdep1
                         255 -> readInstr0 I_impdep2
                         _ -> error $ "unknown opcode " ++ (show c)
            is <- readInstr (i-(length instr))
            return (instr ++ is)

    readExceptionTable :: JCParser [ExceptionInfo]
    readExceptionTable = do
        l <- readu2
        count l readException

    readException :: JCParser ExceptionInfo
    readException = ExceptionInfo <$>
        readu2 <*>
        readu2 <*>
        readu2 <*>
        readu2

    readConstantValue :: JCParser AttrData
    readConstantValue = ConstValue <$> readu2

    readExceptions :: JCParser AttrData
    readExceptions = do
        l <- readu2
        cs <- count l $ readClassName False
        return $ Exceptions cs

    readInnerClasses :: JCParser AttrData
    readInnerClasses = do
        l <- readu2
        ics <- count l readInnerClass
        return $ InnerClasses ics

    readInnerClass :: JCParser InnerClassInfo
    readInnerClass = InnerClassInfo <$>
        readClassName False <*>
        readClassName True <*>
        readName True <*>
        readAccessFlags

    readLineNumberTable :: JCParser AttrData
    readLineNumberTable = do
        l   <- readu2
        lns <- count l readLineNumber
        return $ LineNumbers lns

    readLineNumber :: JCParser LineNumberInfo
    readLineNumber = LineNumberInfo <$>
        readu2 <*>
        readu2

    readLocalVariableTable :: JCParser AttrData
    readLocalVariableTable = do
        l   <- readu2
        lvs <- count l readLocalVariable
        return $ LocalVariables lvs

    readLocalVariable :: JCParser LocalVariableInfo
    readLocalVariable = LocalVariableInfo <$>
        readu2 <*>
        readu2 <*>
        readName False <*>
        readDescriptor <*>
        readu2

    readSourceFile :: JCParser AttrData
    readSourceFile = SourceFile <$> readName False

    readSynthetic :: JCParser AttrData
    readSynthetic = return Synthetic

    readMethods :: JCParser (M.Map (String, String) MethodInfo)
    readMethods = do
        l  <- readu2
        ms <- count l readMethod
        return $ M.fromList $ map (\m -> ((method_name m, method_descriptor m), m)) ms

    readMethod :: JCParser MethodInfo
    readMethod = do
        afs <- readAccessFlags
        mn  <- readName False
        d   <- readDescriptor
        as  <- readAttrs
        case readTypeSig d of
            Left err -> error err
            Right s -> return $ MethodInfo {
                                            method_access_flags   = afs,
                                            method_name           = mn,
                                            method_descriptor     = d,
                                            method_descriptor_sig = s,
                                            method_attributes     = as
                                           }

    readStackMapTable :: JCParser AttrData
    readStackMapTable = do
        l <- readu2
        count l readStackMapFrame
        return StackMaps

    readStackMapFrame :: JCParser ()
    readStackMapFrame = do
        tag <- readu1
        case tag of
            _ | tag >=0 && tag <= 63 -> return ()
            _ | tag >= 64 && tag <= 127 -> count 1 readVerifTypeInfo >> return ()
            247 -> readu2 >> count 1 readVerifTypeInfo >> return ()
            _ | tag >= 248 && tag <= 250 -> readu2 >> return ()
            251 -> readu2 >> return ()
            ft | tag >= 252 && tag <= 254 -> readu2 >> count (ft - 251) readVerifTypeInfo >> return ()
            255 -> do
                readu2
                nls <- readu2
                count nls readVerifTypeInfo
                nss <- readu2
                count nss readVerifTypeInfo
                return ()

    readVerifTypeInfo :: JCParser ()
    readVerifTypeInfo = do
        tag <- readu1
        case tag of
            0 -> return ()
            1 -> return ()
            2 -> return ()
            4 -> return ()
            3 -> return ()
            5 -> return ()
            6 -> return ()
            7 -> readu2 >> return ()
            8 -> readu2 >> return ()

    parseTypeSig :: Parser TypeSig
    parseTypeSig =
        SigVoid    <$ char 'V' <|>
        SigByte    <$ char 'B' <|>
        SigChar    <$ char 'C' <|>
        SigShort   <$ char 'S' <|>
        SigLong    <$ char 'J' <|>
        SigInteger <$ char 'I' <|>
        SigFloat   <$ char 'F' <|>
        SigDouble  <$ char 'D' <|>
        SigClass   <$> (do
            char 'L'
            s <- manyTill anyChar $ char ';'
            return s) <|>
        SigArray   <$> (do
            char '['
            parseTypeSig) <|>
        (do
            char '('
            ss <- many parseTypeSig
            char ')'
            r <- parseTypeSig
            return $ SigFunc ss r)

    readTypeSig :: String -> Either String TypeSig
    readTypeSig s =
        case parse parseTypeSig "" s of
            Left err -> Left $ show err
            Right r -> Right r

    loadClass :: String -> IO (Either ParseError JavaClass)
    loadClass fn = do
        s <- BS.readFile fn
        return $ runParser readClass (array (0,0) []) fn $ BS.unpack s


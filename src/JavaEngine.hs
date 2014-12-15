{-# LANGUAGE MultiWayIf, DeriveDataTypeable, RecordWildCards, NamedFieldPuns, DoAndIfThenElse #-}

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

module JavaEngine (
      bootup
    ) where

    import Control.Applicative
    import Control.Monad.State
    import Control.Monad.Error
    import Control.Exception
    import Control.Concurrent
    import Control.Concurrent.STM
    import GHC.Conc
    import System.Environment
    import System.Directory
    import Data.Array.MArray
    import Data.Array.IO
    import qualified Data.Array as A
    import qualified Data.Set as S
    import qualified Data.Map as M
    import Data.List
    import Data.List.Split
    import Data.Bits
    import Data.Binary.IEEE754
    import Data.Char
    import Data.IORef
    import Data.Typeable

    import JavaClass
    import JavaClassReader

    type JVM a = ErrorT String (StateT JThread IO) a

    newtype NameSpace = NameSpace String deriving (Show, Eq, Ord)

    data JVMEnv = JVMEnv {
                          namespaces :: M.Map NameSpace JCNS
                         } deriving (Eq)

    type JCNS = M.Map String JavaClassObj

    data JavaClassObj = JavaClassObj {
                                      java_class        :: JavaClass,
                                      init_class_loader :: NameSpace,
                                      static_fields     :: M.Map (String, String) (IORef JType)
                                     } deriving (Eq)

    data JThread = JThread {
                            env   :: TVar JVMEnv,
                            stack :: [JFrame]
                           } deriving (Eq)

    data JFrame = JFrame {
                          current_java_class :: JavaClass,
                          current_namespace  :: NameSpace,
                          frame              :: IOArray Int JType,
                          pc                 :: IORef Int,
                          fp                 :: IORef Int,
                          current_method     :: MethodInfo
                         } deriving (Eq)

    data JObject = JObject {
                            fields     :: M.Map String (M.Map (String, String) (IORef JType)),
                            mutex      :: MVar Bool,
                            instanceof :: JavaClass
                           } deriving (Eq)

    data JType =
        JNull
        | JByte Int
        | JChar Char
        | JShort Int
        | JBool Bool
        | JInteger Int
        | JLong Integer
        | JFloat Float
        | JDouble Double
        | JObj JObject
        | JAddr Int
        | JArray (IOArray Int JType)
        | JRef (IORef JType) deriving (Eq)

    data JException = JException {
                                    exception_state :: JThread,
                                    exception_orig  :: Either String SomeException,
                                    exception_data  :: JType,
                                    exception_stack :: [String]
                                   } deriving (Typeable)

    instance Show JException where
       show (JException{..}) =
           show exception_orig ++
           (foldl' (\acc s -> acc ++ (show s) ++ "\n") "" exception_stack)

    instance Exception JException

    objectFullName :: String
    objectFullName = "java/lang/Object"

    classFullName :: String
    classFullName = "java/lang/Class"

    javaHomeEnvName :: String
    javaHomeEnvName = "JAVAHOME"

    classSearchPathEnvName :: String
    classSearchPathEnvName = "CLASSPATH"

    bootupNameSpace :: NameSpace
    bootupNameSpace = NameSpace "bootup"

    mainName :: (String, String)
    mainName = ("main", "(I[Ljava/lang/String;)I")

    getJVMEnvRef :: JVM (TVar JVMEnv)
    getJVMEnvRef = env <$> get

    getJVMEnv :: JVM JVMEnv
    getJVMEnv = getJVMEnvRef >>= (\g -> liftIO $ atomically $ readTVar g)

    getStack :: JVM [JFrame]
    getStack = stack <$> get

    putStack :: [JFrame] -> JVM ()
    putStack st = get >>= (\e -> put $ e{stack = st})

    getTopFrame :: JVM JFrame
    getTopFrame = getStack >>= (\(f:_) -> return f)

    pushFrame :: NameSpace -> (String, String) -> JavaClass -> JVM JFrame
    pushFrame ns mn jc =
        let Just ms = lookupMethodCodeMaxStack  mn jc
            Just ml = lookupMethodCodeMaxLocals mn jc
            Just mi = lookupMethodInfo          mn jc
            s       = ms + ml
         in do
              ffr <- liftIO $ newListArray (0, (s-1)) $ replicate s JNull
              ffp <- liftIO $ newIORef ml
              fpc <- liftIO $ newIORef 0
              let fr = JFrame {
                               current_java_class = jc,
                               current_namespace  = ns,
                               frame              = ffr,
                               pc                 = fpc,
                               fp                 = ffp,
                               current_method     = mi
                              }
               in do
                    st <- getStack
                    putStack (fr:st)
                    return fr
  
    popFrame :: JVM ()
    popFrame = getStack >>= (\(_:st) -> putStack st)

    readFrame :: Int -> JVM JType
    readFrame i = getTopFrame >>= (\top -> liftIO $ flip readArray i $ frame top)

    writeFrame :: Int -> JType -> JVM ()
    writeFrame i o = getTopFrame >>= (\top -> liftIO $ writeArray (frame top) i o)

    getPC :: JVM Int
    getPC = getTopFrame >>= (\top -> liftIO $ readIORef $ pc top)

    putPC :: Int -> JVM ()
    putPC fpc = getTopFrame >>= (\top -> liftIO $ flip writeIORef fpc $ pc top)

    incPC :: Int -> JVM ()
    incPC n = getPC >>= (\fpc -> putPC $ fpc + n)

    getInstr :: JVM Instr
    getInstr = (A.!) <$> (code_instrs <$> getCurrentCodeInfo) <*> getPC

    getFP :: JVM Int
    getFP = getTopFrame >>= (\top -> liftIO $ readIORef $ fp top)

    putFP :: Int -> JVM ()
    putFP ffp = getTopFrame >>= (\top -> liftIO $ flip writeIORef ffp $ fp top)

    incFP :: Int -> JVM ()
    incFP n = getFP >>= (\ffp -> putFP $ ffp + n)

    pushOpand :: JType -> JVM ()
    pushOpand o = getFP >>= (\ffp -> writeFrame ffp o) >> incFP 1

    popOpand :: JVM JType
    popOpand = incFP (-1) >> getFP >>= readFrame

    getCurrentNameSpace :: JVM NameSpace
    getCurrentNameSpace = current_namespace <$> getTopFrame

    getCurrentClass :: JVM JavaClass
    getCurrentClass = current_java_class <$> getTopFrame

    getCurrentMethodInfo :: JVM MethodInfo
    getCurrentMethodInfo = current_method <$> getTopFrame

    getCurrentMethodAttrInfo :: String -> JVM AttrInfo
    getCurrentMethodAttrInfo a = do
        as <- method_attributes <$> getCurrentMethodInfo
        let Just ai = M.lookup a as
         in return ai

    getCurrentCodeInfo :: JVM CodeInfo
    getCurrentCodeInfo = do
        (Code ci) <- attribute_info <$> getCurrentMethodAttrInfo "Code"
        return ci

    currentStack :: JVM [String]
    currentStack = do
        s <- get
        return $ map (\f ->
                        let (SigFunc as r) = method_descriptor_sig $ current_method f
                         in show r ++ (class_this $ current_java_class f) ++
                            "::" ++ (method_name $ current_method f) ++
                            "(" ++ (intercalate ", " $ map show as) ++ ")") $ stack s

    runThread :: JVM Int -> (Int -> IO ()) -> TVar JVMEnv -> IO ThreadId
    runThread body finalizer ger = do
        forkIO $ do
            res <- (try $ (do
                            (r, _) <- (runStateT $ runErrorT body)
                                         (JThread ger [])
                            case r of
                                Left err -> putStrLn err >> return (-1)
                                Right _  -> return 0) :: IO (Either SomeException Int)) >>= evaluate
            case res of
                Left err -> putStrLn (show err) >> finalizer (-1)
                Right ec -> finalizer ec

    mainThread :: JVM Int -> IO Int
    mainThread body = do
        ger <- newTVarIO $ JVMEnv M.empty
        w   <- newEmptyMVar
        runThread body (\ec -> putMVar w ec) ger
        takeMVar w

    newThread ::JVM Int -> JVM ThreadId
    newThread body = do
        ger <- getJVMEnvRef
        liftIO $ runThread body (\_ -> return ()) ger

    resolveClass :: NameSpace -> String -> JVM JavaClassObj
    resolveClass nsn n = do
        ger <- getJVMEnvRef
        c   <- liftIO $ atomically $ do
                  ge <- readTVar ger
                  cs <- let ns = namespaces ge
                         in case M.lookup nsn ns of
                                Nothing -> let cs' = M.empty
                                               ge' = ge{namespaces = M.insert nsn cs' ns}
                                            in do
                                                 writeTVar ger ge'
                                                 return cs'
                                Just cs' -> return cs'
                  case M.lookup n cs of
                     Nothing -> do
                                  jc  <- unsafeIOToSTM $ findClass n
                                  ge' <- readTVar ger
                                  case jc of
                                     Right jc' -> let ns = namespaces ge'
                                                   in do
                                                        jc'' <- unsafeIOToSTM $ initClass (JavaClassObj jc' nsn M.empty)
                                                        let cs'  = M.insert n jc'' cs
                                                            ge'' = ge'{namespaces = M.insert nsn cs' ns}
                                                         in do
                                                              writeTVar ger ge''
                                                              return $ Right jc''
                                     Left err -> return $ Left err
                     Just jc -> return (Right jc)
        case c of 
            Left err -> throwError err
            Right c' -> return c' 

    getEnvOrNot :: String -> IO String
    getEnvOrNot n = do
        r <- try (getEnv n) :: IO (Either SomeException String)
        case r of
            Left _  -> return ""
            Right s -> return s

    findClass :: String -> IO (Either String JavaClass)
    findClass n = do
        jh  <- getEnvOrNot javaHomeEnvName
        ps  <- splitOn ":" <$> getEnvOrNot classSearchPathEnvName
        cns <- foldM
                (\acc p ->
                   let n' = p ++ "/" ++ n
                   in do
                        e <- doesFileExist n' 
                        if e
                        then return (n':acc)
                        else let n'' = n' ++ ".class"
                              in do
                                   e' <- doesFileExist n''
                                   if e'
                                   then return (n'':acc)
                                   else return acc)
                []
                (".":jh:ps)
        if length cns == 0
        then return $ Left $ "cannot find class " ++ n
        else let n' = cns !! 0
              in do
                   res <- loadClass n'
                   case res of
                       Left err -> return $ Left $ show err
                       Right jc -> do
                                     --putStrLn "======================================="
                                     --putStrLn $ show jc
                                     return $ Right jc

    initValue :: TypeSig -> JType
    initValue s = do
        case s of
            SigByte    -> JByte 0
            SigChar    -> JChar '\000'
            SigShort   -> JShort 0
            SigInteger -> JInteger 0
            SigLong    -> JLong 0
            SigFloat   -> JFloat 0
            SigDouble  -> JDouble 0
            SigClass _ -> JNull
            SigArray _ -> JNull
            _          -> JNull

    initClass :: JavaClassObj -> IO JavaClassObj
    initClass jco = do
        let jc = java_class jco
            fs = foldl'
                   (\acc f ->
                      if S.member AccStatic (field_access_flags f)
                      then (f:acc)
                      else acc)
                   []
                   (M.elems $ class_fields jc)
         in do
              fs' <- foldM 
                       (\acc f -> do
                          r <- newIORef $ initValue $ field_descriptor_sig f
                          let mn = (field_name f, field_descriptor f)
                           in return $ M.insert mn r acc)
                       M.empty
                       fs
              return $ jco{static_fields = fs'}

    readStaticField :: (String, String, String) -> JVM JType
    readStaticField (cn, n, d) = do
        nsn <- getCurrentNameSpace
        fs  <- static_fields <$> resolveClass nsn cn
        let Just f = M.lookup (n, d) fs
         in do
              v <- liftIO $ readIORef f
              return v

    writeStaticField :: (String, String, String) -> JType -> JVM ()
    writeStaticField (cn, n, d) v = do
        nsn <- getCurrentNameSpace
        fs  <- static_fields <$> resolveClass nsn cn
        let Just f = M.lookup (n, d) fs
         in liftIO $ writeIORef f v

    allocFields :: String -> JVM (M.Map String (M.Map (String, String) (IORef JType)))
    allocFields cn = do
        nsn <- getCurrentNameSpace
        if cn == ""
        then return M.empty
        else do
            jc <- java_class <$> resolveClass nsn cn
            fs <- foldM
                    (\acc ((n, d), f) -> do
                        r <- liftIO $ newIORef $ initValue $ field_descriptor_sig f
                        return $ M.insert (n, d) r acc)
                    M.empty
                    (M.toList $ class_fields jc)
            fs' <- allocFields (class_super jc)
            return $ M.insert cn fs fs'

    getPureType :: String -> JVM String
    getPureType t =
        case readTypeSig t of
            Left err -> return t
            Right s -> let f = \s' -> case s' of
                                         SigArray as -> f as
                                         _ -> s'
                        in return $ show $ f s

    newObject :: String -> JVM JType
    newObject cn = do
        cn' <- getPureType cn
        case cn' of
          "B" -> return $ JByte 0
          "C" -> return $ JChar '\000'
          "S" -> return $ JShort 0
          "I" -> return $ JInteger 0
          "J" -> return $ JLong 0
          "F" -> return $ JFloat 0
          "D" -> return $ JDouble 0
          _ -> do 
                 nsn <- getCurrentNameSpace
                 jc <- java_class <$> resolveClass nsn cn'
                 fs <- allocFields cn
                 m  <- liftIO $ newEmptyMVar
                 r  <- liftIO $ newIORef (JObj $ JObject fs m jc)
                 return $ JRef r

    newObjectWithInit :: String -> String -> [JType] -> JVM JType
    newObjectWithInit cn d args = do
        o <- newObject cn
        nsn <- getCurrentNameSpace
        invokeMethod nsn (cn, "<init>", d) False (o:args)

    checkCast :: JavaClass -> JavaClass -> JVM Bool
    checkCast to from = do
        nsn <- getCurrentNameSpace
        let checksuper = (\f ->
                           if to == f
                           then return True
                           else case class_super f of
                                  "" -> return False
                                  n -> do
                                         sc <- java_class <$> resolveClass nsn n
                                         checksuper sc)
         in do
             r <- checksuper from
             if r
             then return True
             else let is = class_interfaces from
                   in return $ S.member (class_this to) is

    resolveField :: (String, String, String) -> JObject -> JVM (IORef JType)
    resolveField (cn, n, d) o = do
        nsn <- getCurrentNameSpace
        let Just fs = M.lookup cn $ fields o
         in case M.lookup (n, d) fs of
                Nothing -> do
                             sn <- (class_super . java_class) <$> resolveClass nsn cn
                             if sn == ""
                             then throwError $ "cannot find field " ++ n
                             else resolveField (sn, n, d) o
                Just r -> return r

    readObjField :: (String, String, String) -> JObject -> JVM JType
    readObjField n o = do
        f <- resolveField n o
        v <- liftIO $ readIORef f
        return v

    writeObjField :: (String, String, String) -> JObject -> JType -> JVM ()
    writeObjField n o v = do
        f <- resolveField n o
        liftIO $ writeIORef f v

    resolveMethod :: NameSpace -> (String, String, String) -> Bool -> JVM (String, MethodInfo)
    resolveMethod nsn (cn, n, d) lookupParent = do
        jc <- java_class <$> resolveClass nsn cn
        case lookupMethodInfo (n, d) jc of
            Nothing -> if not lookupParent
                       then throwError $ "cannot find method " ++ n
                       else let sn = class_super jc
                             in if sn == ""
                                then throwError $ "cannot find method " ++ n
                                else resolveMethod nsn (sn, n, d) lookupParent
            Just mi -> return (class_this jc, mi)

    popArgs :: String -> JVM [JType]
    popArgs d =
        case readTypeSig d of
            Left err -> throwError err
            Right (SigFunc ss r) ->
                  mapM (\_ -> popOpand) ss >>= (\l -> return $ reverse l)

    checkArg :: TypeSig -> JType -> JVM ()
    checkArg SigByte (JByte _)       = return () 
    checkArg SigChar (JChar _)       = return () 
    checkArg SigShort (JShort _)     = return () 
    checkArg SigInteger (JInteger _) = return ()
    checkArg SigLong (JLong _)       = return () 
    checkArg SigFloat (JFloat _)     = return () 
    checkArg SigDouble (JDouble _)   = return () 
    checkArg (SigClass _) (JRef _)   = return ()
    checkArg (SigArray _) (JRef _)   = return ()
    checkArg _ _ = throwError "arguments type mismatch"

    pushArgs :: String -> [JType] -> JVM ()
    pushArgs d args = do
        case readTypeSig d of
            Left err -> throwError err
            Right (SigFunc ss r) ->
                  let ss' = if (length ss) == (length args) then ss else (SigClass objectFullName:ss)
                  in do
                     mapM (\(a, b) -> checkArg a b) (zip ss' args)
                     foldM
                       (\i (s, a) -> do
                         case s of
                             SigDouble -> writeFrame i a >> (return $ i + 2)
                             SigLong   -> writeFrame i a >> (return $ i + 2)
                             _         -> writeFrame i a >> (return $ i + 1))
                       0
                       (zip ss' args)
        return ()

    invokeMethod :: NameSpace -> (String, String, String) -> Bool -> [JType] -> JVM JType
    invokeMethod nsn an@(_, _, d) lookupParent args =  do
        (cn, mi) <- resolveMethod nsn an lookupParent
        jc <- java_class <$> resolveClass nsn cn
        let mn   = (method_name mi, method_descriptor mi)
            mafs = method_access_flags mi
         in do
              res <- if S.member AccNative mafs
                     then invokeNativeMethod an args
                     else do
                            pushFrame nsn mn jc
                            pushArgs d args
                            s <- get
                            r <- liftIO (try((runStateT $ runErrorT $ execInstrs) s >>= evaluate) ::
                                   IO (Either SomeException (Either String JType, JThread)))
                            case r of
                                Left e -> do
                                            case fromException e of
                                               Nothing -> do popFrame
                                                             st <- currentStack
                                                             throw $ JException s (Right e) JNull st
                                               Just (JException {exception_state=s', ..}) -> do
                                                  put s'
                                                  popFrame
                                                  throw e
                                Right (r', s') -> do
                                    put s'
                                    popFrame
                                    case r' of
                                        Left err -> throwError err
                                        Right r'' -> return r''
              return res

    printJ :: JType -> JVM ()
    printJ o =
        case o of
            JNull      -> return ()
            JByte v    -> liftIO $ putStr $ show v
            JChar v    -> liftIO $ putStr $ show v
            JShort v   -> liftIO $ putStr $ show v
            JInteger v -> liftIO $ putStr $ show v
            JLong v    -> liftIO $ putStr $ show v
            JFloat v   -> liftIO $ putStr $ show v
            JDouble v  -> liftIO $ putStr $ show v
            JAddr v    -> liftIO $ putStr $ show v
            JArray ar  -> do
              a <- liftIO $ getElems ar
              liftIO $ putStr "["
              foldM
                (\acc v -> printJ v >> (if acc == 1 then return () else liftIO $ putStr ",") >> return (acc-1))
                (length a)
                a
              liftIO $ putStr "]"
              return ()
            JObj o     -> return ()
            v@(JRef r) -> do
              v' <- liftIO $ readIORef r
              case v' of
                (JObj o) -> do
                  nsn <- getCurrentNameSpace
                  v' <- invokeMethod nsn (objectFullName, "toString", "()V") True [v]
                  printJ v'
                _ -> do
                  printJ v'

    invokeNativeMethod :: (String, String, String) -> [JType] -> JVM JType
    invokeNativeMethod n args =
        case n of
            (objectFullName, "printf", "()V") -> mapM printJ args >> return JNull
            _ -> (throwError $ "cannot find native method " ++ (show n)) >> return JNull

    searchMain :: NameSpace -> JVM JavaClassObj
    searchMain nsn = do
        ge <- getJVMEnv
        case M.lookup nsn (namespaces ge) of
            Nothing -> throwError $ "Cannot find namespace: " ++ (show nsn)
            Just ns -> searchMain' $ M.elems ns

    searchMain' :: [JavaClassObj] -> JVM JavaClassObj
    searchMain' cs = do
        -- liftIO $ putStrLn $ show $ map java_class cs
        let cs' = foldl'
                   (\acc jco ->
                     let jc = java_class jco 
                         cafs = class_access_flags jc
                      in if True -- S.member AccPublic cafs
                         then case lookupMethodInfo mainName jc of
                                  Nothing -> acc
                                  Just mi -> let mafs = method_access_flags mi
                                              in if S.member AccPublic mafs && S.member AccStatic mafs
                                                 then (jco:acc)
                                                 else acc
                         else acc
                     )
                   []
                   cs
         in case (length cs') of
               0 -> throwError "Cannot find main method"
               1 -> return (cs' !! 0)
               _ -> throwError "There are more than one main method"

    bootup :: [String] -> IO Int
    bootup cs = mainThread $ do
        mapM (\c -> resolveClass bootupNameSpace c) cs
        jc <- java_class <$> searchMain bootupNameSpace
        let (n, d) = mainName
         in invokeMethod bootupNameSpace (class_this jc, n, d) True []
        return 0

    execInstrs :: JVM JType
    execInstrs = do
        instr <- getInstr
        -- for debug
        -- fpc   <- getPC
        -- ffp   <- getFP
        -- cn    <- class_this <$> getCurrentClass
        -- mn    <- method_name <$> getCurrentMethodInfo
        -- liftIO $ putStrLn $ cn ++ "::" ++ mn ++ " : instr : " ++ (show instr) ++ " : pc : " ++ (show fpc) ++ " : fp : " ++ (show ffp)
        -- 
        s <- get
        r <- liftIO (try((runStateT $ runErrorT $ execInstr instr) s >>= evaluate) ::
                      IO (Either SomeException (Either String (Bool, JType), JThread)))
        case r of
           Left e -> do
               es <- code_exceptions <$> getCurrentCodeInfo
               p  <- getPC
               let ces = foldl'
                           (\acc ei ->
                              if p >= (exception_start_pc ei) && p <= (exception_end_pc ei)
                              then ((exception_handler_pc ei, exception_catch_type ei):acc)
                              else acc)
                           []
                           es
                in case fromException e of
                       Nothing -> do
                            st <- currentStack
                            throw $ JException s (Right e) JNull st
                       Just je@(JException {exception_state=s', ..}) -> do
                            put s'
                            if length ces == 0
                            then throw je
                            else let (p', eni) = ces !! 0
                                  in do
                                       jc <- getCurrentClass
                                       eo <- newObject $ lookupClassName eni jc
                                       pushOpand eo
                                       putPC p'
                                       execInstrs
           Right (r', s') -> do
               put s'
               case r' of
                   Left err -> throwError err
                   Right (cont, ret) -> if cont
                                        then execInstrs
                                        else return ret

    execInstr :: Instr -> JVM (Bool, JType)
    execInstr i = do
        case i of
            I_unknown -> throwError "unknown opcode"
            I_nop -> incPC 1 >> return (True, JNull)
            I_aconst_null -> do
              pushOpand JNull
              incPC 1
              return (True, JNull)
            I_iconst_m1 -> do
              pushOpand $ JInteger (-1)
              incPC 1
              return (True, JNull)
            I_iconst_0 -> do
              pushOpand $ JInteger 0
              incPC 1
              return (True, JNull)
            I_iconst_1 -> do
              pushOpand $ JInteger 1
              incPC 1
              return (True, JNull)
            I_iconst_2 -> do
              pushOpand $ JInteger 2
              incPC 1
              return (True, JNull)
            I_iconst_3 -> do
              pushOpand $ JInteger 3
              incPC 1
              return (True, JNull)
            I_iconst_4 -> do
              pushOpand $ JInteger 4
              incPC 1
              return (True, JNull)
            I_iconst_5 -> do
              pushOpand $ JInteger 5
              incPC 1
              return (True, JNull)
            I_lconst_0 -> do
              pushOpand $ JLong 0
              incPC 1
              return (True, JNull)
            I_lconst_1 -> do
              pushOpand $ JLong 1
              incPC 1
              return (True, JNull)
            I_fconst_0 -> do
              pushOpand $ JFloat 0
              incPC 1
              return (True, JNull)
            I_fconst_1 -> do
              pushOpand $ JFloat 1
              incPC 1
              return (True, JNull)
            I_fconst_2 -> do
              pushOpand $ JFloat 2
              incPC 1
              return (True, JNull)
            I_dconst_0 -> do
              pushOpand $ JDouble 0
              incPC 1
              return (True, JNull)
            I_dconst_1 -> do
              pushOpand $ JDouble 1
              incPC 1
              return (True, JNull)
            I_bipush i -> do
              pushOpand $ JInteger i
              incPC 2
              return (True, JNull)
            I_sipush i -> do
              pushOpand $ JInteger i
              incPC 3
              return (True, JNull)
            I_ldc i -> do
              cp <- class_const_pool <$> getCurrentClass
              case cp A.! i of
                 ConstInteger v -> pushOpand $ JInteger v
                 ConstFloat v   -> pushOpand $ JFloat v
                 -- ConstString v  ->                              -- TODO
              incPC 2
              return (True, JNull)
            I_ldc_w i -> do
              cp <- class_const_pool <$> getCurrentClass
              case cp A.! i of
                 ConstInteger v -> pushOpand $ JInteger v
                 ConstFloat v   -> pushOpand $ JFloat v
                -- ConstString v  ->                              -- TODO
              incPC 3
              return (True, JNull)
            I_ldc2_w i -> do
              cp <- class_const_pool <$> getCurrentClass
              case cp A.! i of
                 ConstLong v   -> pushOpand $ JLong v
                 ConstDouble v -> pushOpand $ JDouble v
              incPC 3
              return (True, JNull)
            I_iload i -> do
              v@(JInteger _) <- readFrame i
              pushOpand v
              incPC 2
              return (True, JNull)
            I_lload i -> do
              v@(JLong _) <- readFrame i
              pushOpand v
              incPC 2
              return (True, JNull)
            I_fload i -> do
              v@(JFloat _) <- readFrame i
              pushOpand v
              incPC 2
              return (True, JNull)
            I_dload i -> do
              v@(JDouble _) <- readFrame i
              pushOpand v
              incPC 2
              return (True, JNull)
            I_aload i -> do
              v@(JRef _) <- readFrame i
              pushOpand v
              incPC 2
              return (True, JNull)
            I_iload_0 -> do
              v@(JInteger _) <- readFrame 0
              pushOpand v
              incPC 1
              return (True, JNull)
            I_iload_1 -> do
              v@(JInteger _) <- readFrame 1
              pushOpand v
              incPC 1
              return (True, JNull)
            I_iload_2 -> do
              v@(JInteger _) <- readFrame 2
              pushOpand v
              incPC 1
              return (True, JNull)
            I_iload_3 -> do
              v@(JInteger _) <- readFrame 3
              pushOpand v
              incPC 1
              return (True, JNull)
            I_lload_0 -> do
              v@(JLong _) <- readFrame 0
              pushOpand v
              incPC 1
              return (True, JNull)
            I_lload_1 -> do
              v@(JLong _) <- readFrame 1
              pushOpand v
              incPC 1
              return (True, JNull)
            I_lload_2 -> do
              v@(JLong _) <- readFrame 2
              pushOpand v
              incPC 1
              return (True, JNull)
            I_lload_3 -> do
              v@(JLong _) <- readFrame 3
              pushOpand v
              incPC 1
              return (True, JNull)
            I_fload_0 -> do
              v@(JFloat _) <- readFrame 0
              pushOpand v
              incPC 1
              return (True, JNull)
            I_fload_1 -> do
              v@(JFloat _) <- readFrame 1
              pushOpand v
              incPC 1
              return (True, JNull)
            I_fload_2 -> do
              v@(JFloat _) <- readFrame 2
              pushOpand v
              incPC 1
              return (True, JNull)
            I_fload_3 -> do
              v@(JFloat _) <- readFrame 3
              pushOpand v
              incPC 1
              return (True, JNull)
            I_dload_0 -> do
              v@(JDouble _) <- readFrame 0
              pushOpand v
              incPC 1
              return (True, JNull)
            I_dload_1 -> do
              v@(JDouble _) <- readFrame 1
              pushOpand v
              incPC 1
              return (True, JNull)
            I_dload_2 -> do
              v@(JDouble _) <- readFrame 2
              pushOpand v
              incPC 1
              return (True, JNull)
            I_dload_3 -> do
              v@(JDouble _) <- readFrame 3
              pushOpand v
              incPC 1
              return (True, JNull)
            I_aload_0 -> do
              v@(JRef _) <- readFrame 0
              pushOpand v
              incPC 1
              return (True, JNull)
            I_aload_1 -> do
              v@(JRef _) <- readFrame 1
              pushOpand v
              incPC 1
              return (True, JNull)
            I_aload_2 -> do
              v@(JRef _) <- readFrame 2
              pushOpand v
              incPC 1
              return (True, JNull)
            I_aload_3 -> do
              v@(JRef _) <- readFrame 3
              pushOpand v
              incPC 1
              return (True, JNull)
            I_iaload -> do
              (JInteger i) <- popOpand
              (JRef r) <- popOpand
              v@(JInteger _) <- liftIO $ do
                                           (JArray a) <- readIORef r
                                           readArray a i
              pushOpand v
              incPC 1
              return (True, JNull)
            I_laload -> do
              (JInteger i) <- popOpand
              (JRef r) <- popOpand
              v@(JLong _) <- liftIO $ do
                                        (JArray a) <- readIORef r
                                        readArray a i
              pushOpand v
              incPC 1
              return (True, JNull)
            I_faload -> do
              (JInteger i) <- popOpand
              (JRef r) <- popOpand
              v@(JFloat _) <- liftIO $ do
                                         (JArray a) <- readIORef r
                                         readArray a i
              pushOpand v
              incPC 1
              return (True, JNull)
            I_daload -> do
              (JInteger i) <- popOpand
              (JRef r) <- popOpand
              v@(JDouble _) <- liftIO $ do
                                          (JArray a) <- readIORef r
                                          readArray a i
              pushOpand v
              incPC 1
              return (True, JNull)
            I_aaload -> do
              (JInteger i) <- popOpand
              (JRef r) <- popOpand
              v@(JRef _) <- liftIO $ do
                                       (JArray a) <- readIORef r
                                       readArray a i
              pushOpand v
              incPC 1
              return (True, JNull)
            I_baload -> do
              (JInteger i) <- popOpand
              (JRef r) <- popOpand
              v@(JBool _) <- liftIO $ do
                                        (JArray a) <- readIORef r
                                        readArray a i
              pushOpand v
              incPC 1
              return (True, JNull)
            I_caload -> do
              (JInteger i) <- popOpand
              (JRef r) <- popOpand
              v@(JChar _) <- liftIO $ do
                                        (JArray a) <- readIORef r
                                        readArray a i
              pushOpand v
              incPC 1
              return (True, JNull)
            I_saload -> do
              (JInteger i) <- popOpand
              (JRef r) <- popOpand
              v@(JShort _) <- liftIO $ do
                                         (JArray a) <- readIORef r
                                         readArray a i
              pushOpand v
              incPC 1
              return (True, JNull)
            I_istore i -> do
              v@(JInteger _) <- popOpand
              writeFrame i v
              incPC 2
              return (True, JNull)
            I_lstore i -> do
              v@(JLong _) <- popOpand
              writeFrame i v
              incPC 2
              return (True, JNull)
            I_fstore i -> do
              v@(JFloat _) <- popOpand
              writeFrame i v
              incPC 2
              return (True, JNull)
            I_dstore i -> do
              v@(JDouble _) <- popOpand
              writeFrame i v
              incPC 2
              return (True, JNull)
            I_astore i -> do
              v <- popOpand
              case v of
                 JRef _ -> return ()
                 JAddr _ -> return ()
              writeFrame i v
              incPC 2
              return (True, JNull)
            I_istore_0 -> do
              v@(JInteger _) <- popOpand
              writeFrame 0 v
              incPC 1
              return (True, JNull)
            I_istore_1 -> do
              v@(JInteger _) <- popOpand
              writeFrame 1 v
              incPC 1
              return (True, JNull)
            I_istore_2 -> do
              v@(JInteger _) <- popOpand
              writeFrame 2 v
              incPC 1
              return (True, JNull)
            I_istore_3 -> do
              v@(JInteger _) <- popOpand
              writeFrame 3 v
              incPC 1
              return (True, JNull)
            I_lstore_0 -> do
              v@(JLong _) <- popOpand
              writeFrame 0 v
              incPC 1
              return (True, JNull)
            I_lstore_1 -> do
              v@(JLong _) <- popOpand
              writeFrame 1 v
              incPC 1
              return (True, JNull)
            I_lstore_2 -> do
              v@(JLong _) <- popOpand
              writeFrame 2 v
              incPC 1
              return (True, JNull)
            I_lstore_3 -> do
              v@(JLong _) <- popOpand
              writeFrame 3 v
              incPC 1
              return (True, JNull)
            I_fstore_0 -> do
              v@(JFloat _) <- popOpand
              writeFrame 0 v
              incPC 1
              return (True, JNull)
            I_fstore_1 -> do
              v@(JFloat _) <- popOpand
              writeFrame 1 v
              incPC 1
              return (True, JNull)
            I_fstore_2 -> do
              v@(JFloat _) <- popOpand
              writeFrame 2 v
              incPC 1
              return (True, JNull)
            I_fstore_3 -> do
              v@(JFloat _) <- popOpand
              writeFrame 3 v
              incPC 1
              return (True, JNull)
            I_dstore_0 -> do
              v@(JDouble _) <- popOpand
              writeFrame 0 v
              incPC 1
              return (True, JNull)
            I_dstore_1 -> do
              v@(JDouble _) <- popOpand
              writeFrame 1 v
              incPC 1
              return (True, JNull)
            I_dstore_2 -> do
              v@(JDouble _) <- popOpand
              writeFrame 2 v
              incPC 1
              return (True, JNull)
            I_dstore_3 -> do
              v@(JDouble _) <- popOpand
              writeFrame 3 v
              incPC 1
              return (True, JNull)
            I_astore_0 -> do
              v <- popOpand
              case v of
                 JRef _ -> return ()
                 JAddr _ -> return ()
              writeFrame 0 v
              incPC 1
              return (True, JNull)
            I_astore_1 -> do
              v <- popOpand
              case v of
                 JRef _ -> return ()
                 JAddr _ -> return ()
              writeFrame 1 v
              incPC 1
              return (True, JNull)
            I_astore_2 -> do
              v <- popOpand
              case v of
                 JRef _ -> return ()
                 JAddr _ -> return ()
              writeFrame 2 v
              incPC 1
              return (True, JNull)
            I_astore_3 -> do
              v <- popOpand
              case v of
                 JRef _ -> return ()
                 JAddr _ -> return ()
              writeFrame 3 v
              incPC 1
              return (True, JNull)
            I_iastore -> do
              v@(JInteger _) <- popOpand
              (JInteger i) <- popOpand
              (JRef r) <- popOpand
              liftIO $ do
                         (JArray a) <- readIORef r
                         writeArray a i v
              incPC 1
              return (True, JNull)
            I_lastore -> do
              v@(JLong _) <- popOpand
              (JInteger i) <- popOpand
              (JRef r) <- popOpand
              liftIO $ do
                         (JArray a) <- readIORef r
                         writeArray a i v
              incPC 1
              return (True, JNull)
            I_fastore -> do
              v@(JFloat _) <- popOpand
              (JInteger i) <- popOpand
              (JRef r) <- popOpand
              liftIO $ do
                         (JArray a) <- readIORef r
                         writeArray a i v
              incPC 1
              return (True, JNull)
            I_dastore -> do
              v@(JDouble _) <- popOpand
              (JInteger i) <- popOpand
              (JRef r) <- popOpand
              liftIO $ do
                         (JArray a) <- readIORef r
                         writeArray a i v
              incPC 1
              return (True, JNull)
            I_aastore -> do
              v@(JRef _) <- popOpand
              (JInteger i) <- popOpand
              (JRef r) <- popOpand
              liftIO $ do
                         (JArray a) <- readIORef r
                         writeArray a i v
              incPC 1
              return (True, JNull)
            I_bastore -> do
              v@(JByte _) <- popOpand
              (JInteger i) <- popOpand
              (JRef r) <- popOpand
              liftIO $ do
                         (JArray a) <- readIORef r
                         writeArray a i v
              incPC 1
              return (True, JNull)
            I_castore -> do
              v@(JChar _) <- popOpand
              (JInteger i) <- popOpand
              (JRef r) <- popOpand
              liftIO $ do
                         (JArray a) <- readIORef r
                         writeArray a i v
              incPC 1
              return (True, JNull)
            I_sastore -> do
              v@(JShort _) <- popOpand
              (JInteger i) <- popOpand
              (JRef r) <- popOpand
              liftIO $ do
                         (JArray a) <- readIORef r
                         writeArray a i v
              incPC 1
              return (True, JNull)
            I_pop -> do
              popOpand
              incPC 1
              return (True, JNull)
            I_pop2 -> do
              popOpand
              popOpand
              incPC 1
              return (True, JNull)
            I_dup -> do
              v <- popOpand
              pushOpand v
              pushOpand v
              incPC 1
              return (True, JNull)
            I_dup_x1 -> do
              v0 <- popOpand
              v1 <- popOpand
              pushOpand v0
              pushOpand v1
              pushOpand v0
              incPC 1
              return (True, JNull)
            I_dup_x2 -> do
              v0 <- popOpand
              v1 <- popOpand
              v2 <- popOpand
              pushOpand v0
              pushOpand v2
              pushOpand v1
              pushOpand v0
              incPC 1
              return (True, JNull)
            I_dup2 -> do
              v0 <- popOpand
              v1 <- popOpand
              pushOpand v1
              pushOpand v0
              pushOpand v1
              pushOpand v0
              incPC 1
              return (True, JNull)
            I_dup2_x1 -> do
              v0 <- popOpand
              v1 <- popOpand
              v2 <- popOpand
              pushOpand v1
              pushOpand v0
              pushOpand v2
              pushOpand v1
              pushOpand v0
              incPC 1
              return (True, JNull)
            I_dup2_x2 -> do
              v0 <- popOpand 
              v1 <- popOpand
              v2 <- popOpand
              v3 <- popOpand
              pushOpand v1
              pushOpand v0
              pushOpand v3
              pushOpand v2
              pushOpand v1
              pushOpand v0
              incPC 1
              return (True, JNull)
            I_swap -> do
              v0 <- popOpand
              v1 <- popOpand
              pushOpand v0
              pushOpand v1
              incPC 1
              return (True, JNull)
            I_iadd -> do
              (JInteger a) <- popOpand
              (JInteger b) <- popOpand
              pushOpand (JInteger (a + b))
              incPC 1
              return (True, JNull)
            I_ladd -> do
              (JLong a) <- popOpand
              (JLong b) <- popOpand
              pushOpand (JLong (a + b))
              incPC 1
              return (True, JNull)
            I_fadd -> do
              (JFloat a) <- popOpand
              (JFloat b) <- popOpand
              pushOpand (JFloat (a + b))
              incPC 1
              return (True, JNull)
            I_dadd -> do
              (JDouble a) <- popOpand
              (JDouble b) <- popOpand
              pushOpand (JDouble (a + b))
              incPC 1
              return (True, JNull)
            I_isub -> do
              (JInteger a) <- popOpand
              (JInteger b) <- popOpand
              pushOpand (JInteger (a - b))
              incPC 1
              return (True, JNull)
            I_lsub -> do
              (JLong a) <- popOpand
              (JLong b) <- popOpand
              pushOpand (JLong (a - b))
              incPC 1
              return (True, JNull)
            I_fsub -> do
              (JFloat a) <- popOpand
              (JFloat b) <- popOpand
              pushOpand (JFloat (a - b))
              incPC 1
              return (True, JNull)
            I_dsub -> do
              (JDouble a) <- popOpand
              (JDouble b) <- popOpand
              pushOpand (JDouble (a - b))
              incPC 1
              return (True, JNull)
            I_imul -> do
              (JInteger a) <- popOpand
              (JInteger b) <- popOpand
              pushOpand (JInteger (a * b))
              incPC 1
              return (True, JNull)
            I_lmul -> do
              (JLong a) <- popOpand
              (JLong b) <- popOpand
              pushOpand (JLong (a * b))
              incPC 1
              return (True, JNull)
            I_fmul -> do
              (JFloat a) <- popOpand
              (JFloat b) <- popOpand
              pushOpand (JFloat (a * b))
              incPC 1
              return (True, JNull)
            I_dmul -> do
              (JDouble a) <- popOpand
              (JDouble b) <- popOpand
              pushOpand (JDouble (a * b))
              incPC 1
              return (True, JNull)
            I_idiv -> do
              (JInteger a) <- popOpand
              (JInteger b) <- popOpand
              pushOpand (JInteger (a `div` b))
              incPC 1
              return (True, JNull)
            I_ldiv -> do
              (JLong a) <- popOpand
              (JLong b) <- popOpand
              pushOpand (JLong (a `div` b))
              incPC 1
              return (True, JNull)
            I_fdiv -> do
              (JFloat a) <- popOpand
              (JFloat b) <- popOpand
              pushOpand (JFloat (a / b))
              incPC 1
              return (True, JNull)
            I_ddiv -> do
              (JDouble a) <- popOpand
              (JDouble b) <- popOpand
              pushOpand (JDouble (a / b))
              incPC 1
              return (True, JNull)
            I_irem -> do
              (JInteger a) <- popOpand
              (JInteger b) <- popOpand
              pushOpand (JInteger (a `rem` b))
              incPC 1
              return (True, JNull)
            I_lrem -> do
              (JLong a) <- popOpand
              (JLong b) <- popOpand
              pushOpand (JLong (a `rem` b))
              incPC 1
              return (True, JNull)
            I_frem -> do
              (JFloat a) <- popOpand
              (JFloat b) <- popOpand
              pushOpand (JFloat 0)   --- TODO
              incPC 1
              return (True, JNull)
            I_drem -> do
              (JDouble a) <- popOpand
              (JDouble b) <- popOpand
              pushOpand (JDouble 0)   --- TODO
              incPC 1
              return (True, JNull)
            I_ineg -> do
              (JInteger v) <- popOpand
              pushOpand (JInteger (-v))
              incPC 1
              return (True, JNull)
            I_lneg -> do
              (JLong v) <- popOpand
              pushOpand (JLong (-v))
              incPC 1
              return (True, JNull)
            I_fneg -> do
              (JFloat v) <- popOpand
              pushOpand (JFloat (-v))
              incPC 1
              return (True, JNull)
            I_dneg -> do
              (JDouble v) <- popOpand
              pushOpand (JDouble (-v))
              incPC 1
              return (True, JNull)
            I_ishl -> do
              (JInteger v) <- popOpand
              (JInteger i) <- popOpand
              pushOpand (JInteger $ shift v i)
              incPC 1
              return (True, JNull)
            I_lshl ->  do
              (JLong v) <- popOpand
              (JInteger i) <- popOpand
              pushOpand (JLong $ shift v i)
              incPC 1
              return (True, JNull)
            I_ishr -> do
              (JInteger v) <- popOpand
              (JInteger i) <- popOpand
              pushOpand (JInteger $ shift v (-i))
              incPC 1
              return (True, JNull)
            I_lshr -> do
              (JLong v) <- popOpand
              (JInteger i) <- popOpand
              pushOpand (JLong $ shift v (-i))
              incPC 1
              return (True, JNull)
            I_iushr -> do
              (JInteger v) <- popOpand
              (JInteger i) <- popOpand
              pushOpand (JInteger $ shift v (-i))  -- TODO
              incPC 1
              return (True, JNull)
            I_lushr -> do
              (JLong v) <- popOpand
              (JInteger i) <- popOpand
              pushOpand (JLong $ shift v (-i))  -- TDOO
              incPC 1
              return (True, JNull)
            I_iand -> do
              (JInteger a) <- popOpand
              (JInteger b) <- popOpand
              pushOpand (JInteger $ a .&. b)
              incPC 1
              return (True, JNull)
            I_land -> do
              (JLong a) <- popOpand
              (JLong b) <- popOpand
              pushOpand (JLong $ a .&. b)
              incPC 1
              return (True, JNull)
            I_ior -> do
              (JInteger a) <- popOpand
              (JInteger b) <- popOpand
              pushOpand (JInteger $ a .|. b)
              incPC 1
              return (True, JNull)
            I_lor -> do
              (JLong a) <- popOpand
              (JLong b) <- popOpand
              pushOpand (JLong $ a .|. b)
              incPC 1
              return (True, JNull)
            I_ixor -> do
              (JInteger a) <- popOpand
              (JInteger b) <- popOpand
              pushOpand (JInteger $ a `xor` b)
              incPC 1
              return (True, JNull)
            I_lxor -> do
              (JLong a) <- popOpand
              (JLong b) <- popOpand
              pushOpand (JLong $ a `xor` b)
              incPC 1
              return (True, JNull)
            I_iinc i c -> do
              (JInteger v) <- readFrame i
              writeFrame i $ JInteger (v + c)
              incPC 3
              return (True, JNull)
            I_i2l -> do
              (JInteger v) <- popOpand
              pushOpand $ JLong $ toInteger v
              incPC 1
              return (True, JNull)
            I_i2f -> do
              (JInteger v) <- popOpand
              pushOpand $ JFloat $ wordToFloat $ fromInteger $ toInteger v
              incPC 1
              return (True, JNull)
            I_i2d -> do
              (JInteger v) <- popOpand
              pushOpand $ JDouble $ wordToDouble $ fromInteger $ toInteger v
              incPC 1
              return (True, JNull)
            I_l2i -> do
              (JLong v) <- popOpand
              pushOpand $ JInteger $ fromInteger v
              incPC 1
              return (True, JNull)
            I_l2f -> do
              (JLong v) <- popOpand
              pushOpand $ JFloat $ wordToFloat $ fromInteger v
              incPC 1
              return (True, JNull)
            I_l2d -> do
              (JLong v) <- popOpand
              pushOpand $ JDouble $ wordToDouble $ fromInteger  v
              incPC 1
              return (True, JNull)
            I_f2i -> do
              (JFloat v) <- popOpand
              pushOpand $ JInteger $ fromInteger $ toInteger $ floatToWord v
              incPC 1
              return (True, JNull)
            I_f2l -> do
              (JFloat v) <- popOpand
              pushOpand $ JLong $ toInteger $ floatToWord v
              incPC 1
              return (True, JNull)
            I_f2d -> do
              (JFloat v) <- popOpand
              pushOpand $ JDouble $ wordToDouble $ fromInteger $ toInteger $ floatToWord v -- TODO
              incPC 1
              return (True, JNull)
            I_d2i -> do
              (JDouble v) <- popOpand
              pushOpand $ JInteger $ fromInteger $ toInteger $ doubleToWord v
              incPC 1
              return (True, JNull)
            I_d2l -> do
              (JDouble v) <- popOpand
              pushOpand $ JLong $ toInteger $ doubleToWord v
              incPC 1
              return (True, JNull)
            I_d2f -> do
              (JDouble v) <- popOpand
              pushOpand $ JFloat $ wordToFloat $ fromInteger $ toInteger $ doubleToWord v -- TODO
              incPC 1
              return (True, JNull)
            I_i2b -> do
              (JInteger v) <- popOpand
              pushOpand $ JByte $ fromInteger $ toInteger v
              incPC 1
              return (True, JNull)
            I_i2c -> do
              (JInteger v) <- popOpand
              pushOpand $ JChar $ chr v
              incPC 1
              return (True, JNull)
            I_i2s -> do
              (JInteger v) <- popOpand
              pushOpand $ JShort v
              incPC 1
              return (True, JNull)
            I_lcmp -> do
              (JLong a) <- popOpand
              (JLong b) <- popOpand
              if | a == b -> pushOpand $ JInteger 0
                 | a >  b -> pushOpand $ JInteger 1
                 | a <  b -> pushOpand $ JInteger (-1)
              incPC 1
              return (True, JNull)
            I_fcmpl -> do
              a <- popOpand
              b <- popOpand
              case a of
                  (JLong a') ->
                      case b of
                          (JLong b') -> if | a' == b' -> pushOpand $ JInteger 0
                                           | a' >  b' -> pushOpand $ JInteger 1
                                           | a' <  b' -> pushOpand $ JInteger (-1)
                          _ -> pushOpand $ JInteger (-1)
                  _ -> pushOpand $ JInteger (-1)
              incPC 1
              return (True, JNull)
            I_fcmpg -> do
              a <- popOpand
              b <- popOpand
              case a of
                  (JLong a') ->
                      case b of
                          (JLong b') -> if | a' == b' -> pushOpand $ JInteger 0
                                           | a' >  b' -> pushOpand $ JInteger 1
                                           | a' <  b' -> pushOpand $ JInteger (-1)
                          _ -> pushOpand $ JInteger 1
                  _ -> pushOpand $ JInteger 1
              incPC 1
              return (True, JNull)
            I_dcmpl -> do
              a <- popOpand
              b <- popOpand
              case a of
                  (JDouble a') ->
                      case b of
                          (JDouble b') -> if | a' == b' -> pushOpand $ JInteger 0
                                             | a' >  b' -> pushOpand $ JInteger 1
                                             | a' <  b' -> pushOpand $ JInteger (-1)
                          _ -> pushOpand $ JInteger (-1)
                  _ -> pushOpand $ JInteger (-1)
              incPC 1
              return (True, JNull)
            I_dcmpg -> do
              a <- popOpand
              b <- popOpand
              case a of
                  (JDouble a') ->
                      case b of
                          (JDouble b') -> if | a' == b' -> pushOpand $ JInteger 0
                                             | a' >  b' -> pushOpand $ JInteger 1
                                             | a' <  b' -> pushOpand $ JInteger (-1)
                          _ -> pushOpand $ JInteger 1
                  _ -> pushOpand $ JInteger 1
              incPC 1
              return (True, JNull)
            I_ifeq p -> do
              (JInteger v) <- popOpand
              if v == 0
              then incPC p
              else incPC 3
              return (True, JNull)
            I_ifne p -> do
              (JInteger v) <- popOpand
              if not (v == 0)
              then incPC p
              else incPC 3
              return (True, JNull)
            I_iflt p -> do
              (JInteger v) <- popOpand
              if v < 0
              then incPC p
              else incPC 3
              return (True, JNull)
            I_ifge p -> do
              (JInteger v) <- popOpand
              if v >= 0
              then incPC p
              else incPC 3
              return (True, JNull)
            I_ifgt p -> do
              (JInteger v) <- popOpand
              if v > 0
              then incPC p
              else incPC 3
              return (True, JNull)
            I_ifle p -> do
              (JInteger v) <- popOpand
              if v <= 0
              then incPC p
              else incPC 3
              return (True, JNull)
            I_if_icmpeq p -> do
              (JInteger b) <- popOpand
              (JInteger a) <- popOpand
              if a == b
              then incPC p
              else incPC 3
              return (True, JNull)
            I_if_icmpne p -> do
              (JInteger b) <- popOpand
              (JInteger a) <- popOpand
              if not (a == b)
              then incPC p
              else incPC 3
              return (True, JNull)
            I_if_icmplt p -> do
              (JInteger b) <- popOpand
              (JInteger a) <- popOpand
              if a < b
              then incPC p
              else incPC 3
              return (True, JNull)
            I_if_icmpge p -> do
              (JInteger b) <- popOpand
              (JInteger a) <- popOpand
              if a >= b
              then incPC p
              else incPC 3
              return (True, JNull)
            I_if_icmpgt p -> do
              (JInteger b) <- popOpand
              (JInteger a) <- popOpand
              if a > b
              then incPC p
              else incPC 3
              return (True, JNull)
            I_if_icmple p -> do
              (JInteger b) <- popOpand
              (JInteger a) <- popOpand
              if a <= b
              then incPC p
              else incPC 3
              return (True, JNull)
            I_if_acmpeq p -> do
              (JRef b) <- popOpand
              (JRef a) <- popOpand
              if a == b
              then incPC p
              else incPC 3
              return (True, JNull)
            I_if_acmpne p -> do
              (JRef b) <- popOpand
              (JRef a) <- popOpand
              if not (a == b)
              then incPC p
              else incPC 3
              return (True, JNull)
            I_goto i -> do
              incPC i
              return (True, JNull)
            I_jsr i -> do
              p <- getPC
              pushOpand (JAddr $ p + 1)
              incPC i
              return (True, JNull)
            I_ret i -> do
              (JAddr p) <- readFrame i
              putPC p
              return (True, JNull)
            I_tableswitch d l h ps -> do
              (JInteger i) <- popOpand
              if i < l || i > h
              then incPC d
              else incPC (ps !! (i-l))
              return (True, JNull) 
            I_lookupswitch d t -> do
              (JInteger k) <- popOpand
              case M.lookup k t of
                  Nothing -> incPC d
                  Just p -> incPC p
              return (True, JNull)
            I_ireturn -> do
              v@(JInteger _) <- popOpand
              return (False, v)
            I_lreturn -> do
              v@(JLong _) <- popOpand
              return (False, v)
            I_freturn -> do
              v@(JFloat _) <- popOpand
              return (False, v)
            I_dreturn -> do
              v@(JDouble _) <- popOpand
              return (False, v)
            I_areturn -> do
              v@(JRef _) <- popOpand
              return (False, v)
            I_return -> do
              return (False, JNull)
            I_getstatic i -> do
              jc <- getCurrentClass
              let fn = lookupFieldName i jc
               in do
                    r <- readStaticField fn
                    pushOpand r
              incPC 3
              return (True, JNull)
            I_putstatic i -> do
              v <- popOpand
              jc <- getCurrentClass
              let fn = lookupFieldName i jc
               in writeStaticField fn v
              incPC 3
              return (True, JNull)
            I_getfield i -> do
              (JRef oref) <- popOpand
              o <- liftIO $ readIORef oref
              jc <- getCurrentClass
              let fn = lookupFieldName i jc
                  (JObj o') = o
               in do
                    v <- readObjField fn o'
                    pushOpand v
              incPC 3
              return (True, JNull)
            I_putfield i -> do
              v <- popOpand
              (JRef oref) <- popOpand
              o <- liftIO $ readIORef oref
              jc <- getCurrentClass
              let fn = lookupFieldName i jc
                  (JObj o') = o
               in writeObjField fn o' v
              incPC 3
              return (True, JNull)
            I_invokevirtual i -> do
              jc <- getCurrentClass
              ns <- getCurrentNameSpace
              let (_, n, d) = lookupMethodName i jc
               in do
                    args <- popArgs d
                    o@(JRef oref) <- popOpand
                    jcn <- (\a -> case a of
                                    (JObj o') -> class_this $ instanceof o'
                                    (JArray _) -> objectFullName) <$> (liftIO $ readIORef oref)
                    let mn = (jcn, n, d)
                     in do
                          r <- invokeMethod ns mn True (o:args)
                          if last d == 'V' then return () else pushOpand r
              incPC 3
              return (True, JNull)
            I_invokespecial i -> do
              jc <- getCurrentClass
              ns <- getCurrentNameSpace
              let mn@(_, _, d) = lookupMethodName i jc
               in do
                    args <- popArgs d
                    o <- popOpand
                    r <- invokeMethod ns mn False (o:args)
                    if last d == 'V' then return () else pushOpand r
              incPC 3
              return (True, JNull)
            I_invokestatic i -> do
              jc <- getCurrentClass
              ns <- getCurrentNameSpace
              let mn@(_, _, d) = lookupMethodName i jc
               in do
                    args <- popArgs d
                    r <- invokeMethod ns mn False args
                    if last d == 'V' then return () else pushOpand r
              incPC 3
              return (True, JNull)
            I_invokeinterface i nargs -> do
              jc <- getCurrentClass
              ns <- getCurrentNameSpace
              let (_, n, d) = lookupInterfaceMethodName i jc
               in do
                    args <- mapM (\_ -> popOpand) (replicate (nargs-1) 0)
                    o@(JRef oref) <- popOpand
                    jcn <- (\a -> case a of
                                     (JObj o') -> class_this $ instanceof o'
                                     (JArray _) -> objectFullName) <$> (liftIO $ readIORef oref)
                    let mn = (jcn, n, d)
                     in do
                          r <- invokeMethod ns mn True (o:args)
                          if last d == 'V' then return () else pushOpand r
              incPC 5
              return (True, JNull)
            I_new i -> do
              ns <- getCurrentNameSpace
              jc <- getCurrentClass
              let n = lookupClassName i jc 
               in do
                    cn <- (class_this . java_class) <$> resolveClass ns n
                    o <- newObject cn
                    pushOpand o
              incPC 3
              return (True, JNull)
            I_newarray i -> do
              (JInteger c) <- popOpand
              let l = replicate c (case i of
                                     4  -> JBool False
                                     5  -> JChar '\000'
                                     6  -> JFloat 0
                                     7  -> JDouble 0
                                     8  -> JByte 0
                                     9  -> JShort 0
                                     10 -> JInteger 0
                                     11 -> JLong 0
                                     _  -> JNull)
               in do
                   a <- liftIO $ newListArray (0, c-1) l
                   ar <- liftIO $ newIORef $ JArray a
                   pushOpand $ JRef ar
              incPC 2
              return (True, JNull)
            I_anewarray i -> do
              ns <- getCurrentNameSpace
              jc <- getCurrentClass
              let cn = lookupClassName i jc 
               in do
                    (JInteger c) <- popOpand
                    ol <- mapM newObject (replicate c cn)
                    aa <- liftIO $ newListArray (0, c -1) ol
                    ar <- liftIO $ newIORef $ JArray aa
                    pushOpand $ JRef ar
              incPC 3
              return (True, JNull)
            I_arraylength -> do
              (JRef r) <- popOpand
              l <- liftIO $ do
                         (JArray a) <- readIORef r
                         (0, h) <- getBounds a
                         return $ h + 1
              pushOpand $ JInteger l
              incPC 1
              return (True, JNull)
            I_athrow -> do
              v@(JRef _) <- popOpand
              s  <- get
              st <- currentStack
              throw $ JException s (Left "JException") v st
            I_checkcast i -> do
              jc <- getCurrentClass
              nsn <- getCurrentNameSpace
              t <- java_class <$> (resolveClass nsn $ lookupClassName i jc)
              v@(JRef r) <- popOpand
              (JObj f) <- liftIO $ readIORef r
              r <- checkCast t $ instanceof f
              if r
              then pushOpand v
              else do
                     s  <- get
                     st <- currentStack
                     throw $ JException s (Left "cast failed") v st
              incPC 1
              return (True, JNull)
            I_instanceof i -> do
              jc <- getCurrentClass
              nsn <- getCurrentNameSpace
              t <- java_class <$> (resolveClass nsn $ lookupClassName i jc)
              (JRef r) <- popOpand
              (JObj f) <- liftIO $ readIORef r
              res <- checkCast t $ instanceof f
              pushOpand (JInteger $ if res then 1 else 0)
              incPC 1
              return (True, JNull)
            I_monitorenter -> do
              (JRef r) <- popOpand
              (JObj o) <- liftIO $ readIORef r
              liftIO $ putMVar (mutex o) True
              incPC 1
              return (True, JNull)
            I_monitorexit -> do
              (JRef r) <- popOpand
              (JObj o) <- liftIO $ readIORef r
              liftIO $ takeMVar (mutex o)
              incPC 1
              return (True, JNull)
            I_multianewarray i d -> do
              jc <- getCurrentClass
              nsn <- getCurrentNameSpace
              ds <- mapM (\_-> do
                           (JInteger d') <- popOpand
                           return d')
                         (replicate d 0) >>= (\l -> return $ reverse l)
              let cn = lookupClassName i jc
                  f  = (\i' ->
                            if i' == 0
                            then newObject
                            else let f' = f (i'-1)
                                  in (\cn' -> do
                                         let c = ds !! (i' - 1)
                                          in do
                                              ol <- mapM f' (replicate c cn')
                                              aa <- liftIO $ newListArray (0, c -1) ol
                                              ar <- liftIO $ newIORef $ JArray aa
                                              return $ JRef ar))
               in do
                    ma <- (f d) cn
                    pushOpand ma
                    incPC 4
                    return (True, JNull)
            I_ifnull p -> do
              v <- popOpand
              if v == JNull
              then incPC p
              else incPC 3
              return (True, JNull)
            I_ifnonnull p -> do
              v <- popOpand
              if not (v == JNull)
              then incPC p
              else incPC 3
              return (True, JNull)
            I_goto_w i -> do
              incPC i
              return (True, JNull)
            I_jsr_w i -> do
              p <- getPC
              pushOpand (JAddr $ p + 1)
              incPC i
              return (True, JNull)
            I_iload_w i -> do
              v@(JInteger _) <- readFrame i
              pushOpand v
              incPC 3
              return (True, JNull)
            I_lload_w i -> do
              v@(JLong _) <- readFrame i
              pushOpand v
              incPC 3
              return (True, JNull)
            I_fload_w i -> do
              v@(JFloat _) <- readFrame i
              pushOpand v
              incPC 3
              return (True, JNull)
            I_dload_w i -> do
              v@(JDouble _) <- readFrame i
              pushOpand v
              incPC 3
              return (True, JNull)
            I_aload_w i -> do
              v@(JRef _) <- readFrame i
              pushOpand v
              incPC 3
              return (True, JNull)
            I_istore_w i -> do
              v@(JInteger _) <- popOpand
              writeFrame i v
              incPC 3
              return (True, JNull)
            I_lstore_w i -> do
              v@(JLong _) <- popOpand
              writeFrame i v
              incPC 3
              return (True, JNull)
            I_fstore_w i -> do
              v@(JFloat _) <- popOpand
              writeFrame i v
              incPC 3
              return (True, JNull)
            I_dstore_w i -> do
              v@(JDouble _) <- popOpand
              writeFrame i v
              incPC 3
              return (True, JNull)
            I_astore_w i -> do
              v@(JRef _) <- popOpand
              writeFrame i v
              incPC 3
              return (True, JNull)
            I_iinc_w i c -> do
              (JInteger v) <- readFrame i
              writeFrame i $ JInteger (v + c)
              incPC 4
              return (True, JNull)
            I_ret_w i -> do
              (JAddr p) <- readFrame i
              putPC p
              return (True, JNull)
            I_breakpoint -> do
              incPC 1
              return (True, JNull)
            I_impdep1 -> do
              incPC 1
              return (True, JNull)
            I_impdep2 -> do
              incPC 1
              return (True, JNull)
            a -> throwError $ "unknown opcode " ++ (show a)

        

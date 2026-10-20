{- | Frames behind tables: the registry a DataFrame display books its value
into, the slot-carrying table markup, and the query the grid's expression box
runs against a booked frame. Spliced into the session after display.hs.
-}

{- | A capped table that remembers the frame it came from, so the grid's
expression box can re-shape that frame. @query@ names the function the box
goes through, typed to the frame so an expression infers against it; it
wraps the untyped '_sabelaFrameQuery'.
-}
_sabelaTableWith ::
    SabelaBase.String ->
    SabelaBase.String ->
    [SabelaBase.String] ->
    [SabelaBase.String] ->
    [[SabelaBase.String]] ->
    SabelaBase.Int ->
    Rich
_sabelaTableWith query slot hs tys shown total =
    html (_sabelaTableHtmlWith query slot hs tys shown total)

{- | The frames the cells last displayed, keyed @cell#n@. A table's expression
box names one of these, and the kernel applies the expression to it.
-}
_sabelaFrameRef :: SabelaIORef.IORef [(SabelaBase.String, Data.Dynamic.Dynamic)]
_sabelaFrameRef = SabelaUnsafe.unsafePerformIO _sabelaFrameRefInit

{- | The registry must outlive this block, which is entered again before every
cell: the first evaluation pins the ref with a stable pointer and parks its
address in the process environment, and every later one reads it back.
-}
_sabelaFrameRefInit ::
    SabelaBase.IO (SabelaIORef.IORef [(SabelaBase.String, Data.Dynamic.Dynamic)])
_sabelaFrameRefInit = do
    parked <- System.Environment.lookupEnv "SABELA_FRAME_REF"
    case SabelaBase.maybe [] (\s -> [a | (a, "") <- SabelaBase.reads s]) parked of
        (addr : _) ->
            Foreign.StablePtr.deRefStablePtr
                (Foreign.StablePtr.castPtrToStablePtr (Foreign.Ptr.wordPtrToPtr addr))
        [] -> do
            ref <- SabelaIORef.newIORef []
            sp <- Foreign.StablePtr.newStablePtr ref
            System.Environment.setEnv
                "SABELA_FRAME_REF"
                ( SabelaBase.show
                    (Foreign.Ptr.ptrToWordPtr (Foreign.StablePtr.castStablePtrToPtr sp))
                )
            SabelaBase.return ref

_sabelaFrameCell :: SabelaBase.String -> SabelaBase.String
_sabelaFrameCell = SabelaBase.takeWhile (SabelaBase./= '#')

-- | Hold a value the current cell is displaying, and return its slot key.
_sabelaFrameSlot ::
    (Data.Typeable.Typeable a) => a -> SabelaBase.IO SabelaBase.String
_sabelaFrameSlot v = do
    cid <- SabelaIORef.readIORef _sabelaCellIdRef
    slots <- SabelaIORef.readIORef _sabelaFrameRef
    let mine = SabelaBase.filter (\(k, _) -> _sabelaFrameCell k SabelaBase.== cid) slots
        key = cid SabelaBase.++ "#" SabelaBase.++ SabelaBase.show (SabelaBase.length mine)
    SabelaIORef.writeIORef _sabelaFrameRef ((key, Data.Dynamic.toDyn v) : slots)
    SabelaBase.return key

-- | Forget a cell's held frames, before it runs again.
_sabelaFrameReset :: SabelaBase.String -> SabelaBase.IO ()
_sabelaFrameReset cid =
    SabelaIORef.modifyIORef
        _sabelaFrameRef
        (SabelaBase.filter (\(k, _) -> _sabelaFrameCell k SabelaBase./= cid))

{- | Apply an expression to a held frame and display what it gives. The slot
list is put back afterwards, so a frame the result displays is not kept.
-}
_sabelaFrameQuery ::
    (Data.Typeable.Typeable a, Display b) =>
    SabelaBase.String ->
    (a -> b) ->
    SabelaBase.IO ()
_sabelaFrameQuery key f = do
    slots <- SabelaIORef.readIORef _sabelaFrameRef
    case SabelaBase.lookup key slots of
        SabelaBase.Nothing ->
            SabelaBase.fail "this table's frame is no longer held; run its cell again"
        SabelaBase.Just dyn -> case Data.Dynamic.fromDynamic dyn of
            SabelaBase.Nothing ->
                SabelaBase.fail
                    ( "the table holds a "
                        SabelaBase.++ SabelaBase.show (Data.Dynamic.dynTypeRep dyn)
                        SabelaBase.++ ", which this expression does not take"
                    )
            SabelaBase.Just v -> display (f v)
    SabelaIORef.writeIORef _sabelaFrameRef slots

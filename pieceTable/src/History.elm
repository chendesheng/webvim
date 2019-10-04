module History exposing
    ( History
    , StateHistory
    , commit
    , empty
    , exec
    , getHistory
    , getState
    , isDirty
    , mapState
    , redo
    , save
    , undo
    )


type alias Undo patch =
    List patch


type alias Redo patch =
    Undo patch


emptyUndo : Undo patch
emptyUndo =
    []


type StateHistory state patch
    = StateHistory state (History patch)


type alias History patch =
    { undoes : List (Undo patch)
    , pending : Undo patch
    , redoes : List (Redo patch)
    , savePoint : Int
    }


getState : StateHistory state patch -> state
getState (StateHistory state _) =
    state


getHistory : StateHistory state patch -> History patch
getHistory (StateHistory _ history) =
    history


mapState : (state -> state) -> StateHistory state patch -> StateHistory state patch
mapState fn (StateHistory state history) =
    StateHistory (fn state) history


empty : state -> StateHistory state patch
empty state =
    StateHistory state
        { undoes = []
        , redoes = []
        , pending = emptyUndo
        , savePoint = 0
        }


addPending : (patch -> patch -> List patch) -> patch -> History patch -> History patch
addPending mergePatch patch history =
    { history
        | pending =
            case history.pending of
                x :: xs ->
                    mergePatch patch x ++ xs

                _ ->
                    [ patch ]
    }


exec :
    (patch -> state -> ( state, patch ))
    -> (patch -> patch -> List patch)
    -> patch
    -> StateHistory state patch
    -> StateHistory state patch
exec apply merge patch (StateHistory state history) =
    let
        ( state1, patch1 ) =
            apply patch state
    in
    StateHistory state1 <| addPending merge patch1 history


commit : StateHistory state patch -> StateHistory state patch
commit ((StateHistory state history) as s) =
    let
        { pending, undoes } =
            history
    in
    case pending of
        [] ->
            s

        _ ->
            StateHistory state
                ({ history
                    | undoes = pending :: undoes
                    , pending = emptyUndo
                    , redoes = []
                    , savePoint = history.savePoint + 1
                 }
                 --|> Debug.log "after commit"
                )


applyList :
    (patch -> state -> ( state, patch ))
    -> Undo patch
    -> state
    -> ( state, Redo patch )
applyList apply patches buf =
    let
        ( buf3, patches2 ) =
            List.foldl
                (\patch ( buf1, patches1 ) ->
                    let
                        ( buf2, patch1 ) =
                            apply patch buf1
                    in
                    ( buf2, patch1 :: patches1 )
                )
                ( buf, [] )
                patches
    in
    ( buf3, List.reverse patches2 )


undo :
    (patch -> state -> ( state, patch ))
    -> StateHistory state patch
    -> StateHistory state patch
undo apply ((StateHistory state history) as s) =
    case history.undoes of
        undoItem :: undoes ->
            let
                ( state1, redoItem ) =
                    applyList apply undoItem state
            in
            StateHistory state1
                ({ history
                    | undoes = undoes
                    , redoes = redoItem :: history.redoes
                    , savePoint = history.savePoint - 1
                 }
                 --|> Debug.log "after undo"
                )

        _ ->
            s


redo :
    (patch -> state -> ( state, patch ))
    -> StateHistory state patch
    -> StateHistory state patch
redo apply ((StateHistory state history) as s) =
    case history.redoes of
        redoItem :: redoes ->
            let
                ( state1, undoItem ) =
                    applyList apply redoItem state
            in
            StateHistory state1
                { history
                    | redoes = redoes
                    , undoes = undoItem :: history.undoes
                    , savePoint = history.savePoint + 1
                }

        _ ->
            s


save : StateHistory state patch -> StateHistory state patch
save (StateHistory state history) =
    StateHistory state { history | savePoint = 0 }


isDirty : StateHistory state patch -> Bool
isDirty (StateHistory _ history) =
    history.savePoint /= 0

type Digit =
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine

type Cell =
    | EmptyCell
    | FixedValue of Digit
    | Definitely of Digit
    | MaybeTwo of Digit * Digit
    | MaybeThree of Digit * Digit * Digit
    | MaybeFour of Digit * Digit * Digit * Digit
    | MaybeFive of Digit * Digit * Digit * Digit * Digit
    | MaybeSix of Digit * Digit * Digit * Digit * Digit * Digit
    | MaybeSeven of Digit * Digit * Digit * Digit * Digit * Digit * Digit
    | MaybeEight of Digit * Digit * Digit * Digit * Digit * Digit * Digit * Digit
    | MaybeNine of Digit * Digit * Digit * Digit * Digit * Digit * Digit * Digit * Digit


type Cluster = Cluster of Cell * Cell * Cell * Cell * Cell * Cell * Cell * Cell * Cell

type GameView =
    | Blocks of Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster
    | Rows of Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster
    | Columns of Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster

type UserAction =
    | EnterDigit of Digit
    | Right
    | Left
    | Up
    | Down
    | Quit
    | StartGame

type Game =
    | SettingUp of (int * int) * GameView
    | Solving of (int * int) * GameView
    | Solved of GameView

let first (Cluster (x,_,_,_,_,_,_,_,_)) = x
let second (Cluster (_,x,_,_,_,_,_,_,_)) = x
let third (Cluster (_,_,x,_,_,_,_,_,_)) = x
let fourth (Cluster (_,_,_,x,_,_,_,_,_)) = x
let fifth (Cluster (_,_,_,_,x,_,_,_,_)) = x
let sixth (Cluster (_,_,_,_,_,x,_,_,_)) = x
let seventh(Cluster (_,_,_,_,_,_,x,_,_)) = x
let eighth (Cluster (_,_,_,_,_,_,_,x,_)) = x
let ninth (Cluster (_,_,_,_,_,_,_,_,x)) = x

let mapNine fn (a,b,c,d,e,f,g,h,i) =
    (fn a, fn b, fn c, fn d, fn e, fn f, fn g, fn h, fn i)

let straightenBlocks x y z f0 f1 f2 =
    Cluster
        ( f0 x, f1 x, f2 x
        , f0 y, f1 y, f2 y
        , f0 z, f1 z, f2 z
        )


let viewAsRows board =
    (function
    | Rows _ -> board
    | Blocks (a,b,c,d,e,f,g,h,i) ->
        Rows
            ( straightenBlocks a b c first second third
            , straightenBlocks a b c fourth fifth sixth
            , straightenBlocks a b c seventh eighth ninth
            , straightenBlocks d e f first second third
            , straightenBlocks d e f fourth fifth sixth
            , straightenBlocks d e f seventh eighth ninth
            , straightenBlocks g h i first second third
            , straightenBlocks g h i fourth fifth sixth
            , straightenBlocks g h i seventh eighth ninth
            )
    | Columns (a,b,c,d,e,f,g,h,i) ->
        Rows
            ( Cluster <| mapNine first (a,b,c,d,e,f,g,h,i)
            , Cluster <| mapNine second (a,b,c,d,e,f,g,h,i)
            , Cluster <| mapNine third (a,b,c,d,e,f,g,h,i)
            , Cluster <| mapNine fourth (a,b,c,d,e,f,g,h,i)
            , Cluster <| mapNine fifth (a,b,c,d,e,f,g,h,i)
            , Cluster <| mapNine sixth (a,b,c,d,e,f,g,h,i)
            , Cluster <| mapNine seventh (a,b,c,d,e,f,g,h,i)
            , Cluster <| mapNine eighth (a,b,c,d,e,f,g,h,i)
            , Cluster <| mapNine ninth (a,b,c,d,e,f,g,h,i)
            )
    ) board

let testBoard =
    let newCluster =
        Cluster
            ( Definitely One, Definitely Two, Definitely Three
            , Definitely Four, Definitely Five, Definitely Six
            , Definitely Seven, Definitely Eight, Definitely Nine
            )
    Blocks
        ( newCluster, newCluster, newCluster
        , newCluster, newCluster, newCluster
        , newCluster, newCluster, newCluster
        )

let newBoard =
    let newCluster = 
        Cluster
            ( EmptyCell,EmptyCell,EmptyCell
            , EmptyCell,EmptyCell,EmptyCell
            , EmptyCell,EmptyCell,EmptyCell
            )

    Blocks
        ( newCluster, newCluster, newCluster
        , newCluster, newCluster, newCluster
        , newCluster, newCluster, newCluster
        )

let viewAsColumns board =
    (function
    | Columns _ -> board
    | Blocks (a,b,c,d,e,f,g,h,i) ->
        Columns
            ( straightenBlocks a d g first fourth seventh
            , straightenBlocks a d g second fifth eighth
            , straightenBlocks a d g third sixth ninth
            , straightenBlocks b e h first fourth seventh
            , straightenBlocks b e h second fifth eighth
            , straightenBlocks b e h third sixth ninth
            , straightenBlocks c f i first fourth seventh
            , straightenBlocks c f i second fifth eighth
            , straightenBlocks c f i third sixth ninth
            )
    | Rows (a,b,c,d,e,f,g,h,i) ->
        Columns
            (
                Cluster <| mapNine first (a,b,c,d,e,f,g,h,i),
                Cluster <| mapNine second (a,b,c,d,e,f,g,h,i),
                Cluster <| mapNine third (a,b,c,d,e,f,g,h,i),
                Cluster <| mapNine fourth (a,b,c,d,e,f,g,h,i),
                Cluster <| mapNine fifth (a,b,c,d,e,f,g,h,i),
                Cluster <| mapNine sixth (a,b,c,d,e,f,g,h,i),
                Cluster <| mapNine seventh (a,b,c,d,e,f,g,h,i),
                Cluster <| mapNine eighth (a,b,c,d,e,f,g,h,i),
                Cluster <| mapNine ninth (a,b,c,d,e,f,g,h,i)
            )
    ) board

let viewAsBlocks = viewAsRows

let addLeft digit cell =
    (function
    | FixedValue a -> cell
    | EmptyCell -> Definitely digit
    | Definitely a -> MaybeTwo (digit, a)
    | MaybeTwo (a,b) -> MaybeThree (digit, a,b)
    | MaybeThree (a,b,c) -> MaybeFour (digit, a,b,c)
    | MaybeFour (a,b,c,d) -> MaybeFive (digit, a,b,c,d)
    | MaybeFive (a,b,c,d,e) -> MaybeSix (digit, a,b,c,d,e)
    | MaybeSix (a,b,c,d,e,f) -> MaybeSeven (digit, a,b,c,d,e,f)
    | MaybeSeven (a,b,c,d,e,f,g) -> MaybeEight (digit, a,b,c,d,e,f,g)
    | MaybeEight (a,b,c,d,e,f,g,h) -> MaybeNine (digit,a,b,c,d,e,f,g,h)
    | MaybeNine _ -> cell
    ) cell

let trimLeft cell =
    (function
    | EmptyCell -> cell
    | FixedValue _ -> cell
    | Definitely _ -> EmptyCell
    | MaybeTwo (_,a) -> Definitely a
    | MaybeThree (_,a,b) -> MaybeTwo (a,b)
    | MaybeFour (_,a,b,c) -> MaybeThree (a,b,c)
    | MaybeFive (_,a,b,c,d) -> MaybeFour (a,b,c,d)
    | MaybeSix (_,a,b,c,d,e) -> MaybeFive (a,b,c,d,e)
    | MaybeSeven (_,a,b,c,d,e,f) -> MaybeSix (a,b,c,d,e,f)
    | MaybeEight (_,a,b,c,d,e,f,g) -> MaybeSeven (a,b,c,d,e,f,g)
    | MaybeNine (_,a,b,c,d,e,f,g,h) -> MaybeEight (a,b,c,d,e,f,g,h)
    ) cell

type CouldExist<'something> =
    | Value of 'something
    | Nothing

let peek cell = 
    (function
    | EmptyCell -> Nothing
    | FixedValue x -> Value x
    | Definitely x -> Value x
    | MaybeTwo (x,_) -> Value x
    | MaybeThree (x,_,_) -> Value x
    | MaybeFour (x,_,_,_) -> Value x
    | MaybeFive (x,_,_,_,_) -> Value x
    | MaybeSix (x,_,_,_,_,_) -> Value x
    | MaybeSeven (x,_,_,_,_,_,_) -> Value x
    | MaybeEight (x,_,_,_,_,_,_,_) -> Value x
    | MaybeNine (x,_,_,_,_,_,_,_,_) ->Value x
    ) cell


let andThen f v =
    (function
    | Nothing -> Nothing
    | Value v -> Value (f v)
    ) v

let orElse defaultValue x =
    (function
    | Nothing -> defaultValue
    | Value x -> x
    ) x


let exists x cell =
    let rec realExists remaining =
        peek remaining
        |> andThen (fun v -> v = x || realExists (trimLeft remaining))
        |> orElse false
    (function
    | FixedValue v -> v = x
    | _ -> realExists cell
    ) cell

let insert x cell =
    let rec doInsert remaining =
        peek remaining
        |> andThen (fun item ->
            if item < x then
                addLeft item (doInsert (trimLeft remaining))
            elif item = x then
                remaining
            else
                addLeft x remaining
            )
        |> orElse (Definitely x)
    (function
    | FixedValue _ -> cell
    | _ -> doInsert cell
    ) cell


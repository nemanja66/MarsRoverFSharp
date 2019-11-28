module Domain

let (>>=) x f = Result.bind f x

[<RequireQualifiedAccess>]
type Coordinate =
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten

    override c.ToString() = 
        match c with 
        | One -> "1"
        | Two -> "2"
        | Three -> "3"
        | Four -> "4"
        | Five -> "5"
        | Six -> "6"
        | Seven -> "7"
        | Eight -> "8"
        | Nine -> "9"
        | Ten -> "10"

[<RequireQualifiedAccess>]
module Coordinate = 
    
    let generateCoordinateSuccessor coordinate =
        match coordinate with
        | Coordinate.One -> Coordinate.Two
        | Coordinate.Two -> Coordinate.Three
        | Coordinate.Three -> Coordinate.Four
        | Coordinate.Four -> Coordinate.Five
        | Coordinate.Five -> Coordinate.Six
        | Coordinate.Six -> Coordinate.Seven
        | Coordinate.Seven -> Coordinate.Eight
        | Coordinate.Eight -> Coordinate.Nine
        | Coordinate.Nine -> Coordinate.Ten
        | Coordinate.Ten -> Coordinate.One
    
    let generateCoordinatePredecessor coordinate =
        match coordinate with
        | Coordinate.Ten -> Coordinate.Nine
        | Coordinate.Nine ->Coordinate. Eight
        | Coordinate.Eight -> Coordinate.Seven
        | Coordinate.Seven -> Coordinate.Six
        | Coordinate.Six -> Coordinate.Five
        | Coordinate.Five -> Coordinate.Four
        | Coordinate.Four -> Coordinate.Three
        | Coordinate.Three -> Coordinate.Two
        | Coordinate.Two -> Coordinate.One
        | Coordinate.One -> Coordinate.Ten

[<RequireQualifiedAccess>]
type Direction =
    | North
    | South
    | East
    | West

    override d.ToString() = 
        match d with
        | North -> "N"
        | South -> "S"
        | East -> "E"
        | West -> "W"

[<RequireQualifiedAccess>]
module Direction = 
    let rotateLeft =
        function
        | Direction.North -> Direction.West
        | Direction.South -> Direction.East
        | Direction.East -> Direction.North
        | Direction.West -> Direction.South

    let rotateRight =
        function
        | Direction.North -> Direction.East
        | Direction.South -> Direction.West
        | Direction.East  -> Direction.South
        | Direction.West  -> Direction.North

type Obstacle = {
    x: Coordinate
    y: Coordinate
} 

type Position = {
    x: Coordinate
    y: Coordinate
    direction: Direction
}

[<RequireQualifiedAccess>]
module Position =
    let rotateLeft p = 
        { p with direction = Direction.rotateLeft p.direction }
    let rotateRight p = 
        { p with direction = Direction.rotateRight p.direction }

[<RequireQualifiedAccess>]
type Command =
    | RotateLeft
    | RotateRight
    | Move

let calculateNewCoordinates position = 
    match position.direction with
    | Direction.North -> {position with y = Coordinate.generateCoordinateSuccessor position.y}
    | Direction.South -> {position with y = Coordinate.generateCoordinatePredecessor position.y}
    | Direction.East  -> {position with x = Coordinate.generateCoordinateSuccessor position.x}
    | Direction.West  -> {position with x = Coordinate.generateCoordinatePredecessor position.x}

let detectObstacle: Obstacle list -> Obstacle -> Obstacle option =
        fun obstacles maybeObstacle ->
            if List.contains maybeObstacle obstacles  then      
                {x=maybeObstacle.x; y=maybeObstacle.y} |> Some
                else None

let tryApplyCommand: Obstacle list -> Position -> Result<Position, Obstacle*Direction> =
    fun obstacles nextPosition ->
        match detectObstacle obstacles {x=nextPosition.x; y=nextPosition.y} with
        | Some obstacle -> Error ({x=obstacle.x; y=obstacle.y}, nextPosition.direction)
        | None -> Ok {x=nextPosition.x; y=nextPosition.y; direction = nextPosition.direction}

let toFunc =
    function
    | Command.RotateLeft -> Position.rotateLeft
    | Command.RotateRight -> Position.rotateRight
    | Command.Move -> calculateNewCoordinates
    
let move command obstacles position =
    position
    |> command
    |> tryApplyCommand obstacles

let parseInput chars =
    let commands: Command list = List.Empty 
    Seq.toList chars |> List.fold (fun commands char ->
        match char with
        | 'L' -> Command.RotateLeft :: commands
        | 'R' -> Command.RotateRight :: commands
        | 'M' -> Command.Move :: commands
        |  _  -> commands
        ) commands |> List.rev

let formatOutput: Result<Position, Obstacle*Direction> -> string =
    fun result ->
        match result with
        | Ok p -> sprintf "%O:%O:%O" p.x p.y p.direction
        | Error (o, d) -> sprintf "O:%O:%O:%O" o.x o.y d

let execute position obstacles commands =
    parseInput commands
    |> List.map toFunc
    |> List.fold (fun position command -> 
        position >>= move command obstacles) 
        (Ok position)
    |> formatOutput

                            

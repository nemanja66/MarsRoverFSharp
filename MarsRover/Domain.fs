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

type Location = {
    x: Coordinate
    y: Coordinate
} 

type Position = {
    location: Location
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
    let point = position.location
    let newLocation = 
        match position.direction with
        | Direction.North -> 
            { point with y = Coordinate.generateCoordinateSuccessor point.y }
        | Direction.South -> 
            { point with y = Coordinate.generateCoordinatePredecessor point.y } 
        | Direction.East  -> 
            { point with x = Coordinate.generateCoordinateSuccessor point.x } 
        | Direction.West  -> 
            { point with x = Coordinate.generateCoordinatePredecessor point.x }
    { position with location = newLocation }

let toFunc =
    function
    | Command.RotateLeft -> Position.rotateLeft
    | Command.RotateRight -> Position.rotateRight
    | Command.Move -> calculateNewCoordinates
    
let move command obstacles position =
    let newPosition = position |> command
    let canMove = List.contains newPosition.location obstacles
    newPosition 
    |> if canMove then Error else Ok 

let parseInput chars =
    let commands: Command list = List.Empty 
    Seq.toList chars 
    |> List.fold (fun commands char ->
        match char with
        | 'L' -> Command.RotateLeft :: commands
        | 'R' -> Command.RotateRight :: commands
        | 'M' -> Command.Move :: commands
        |  _  -> commands
        ) commands 
    |> List.rev

let formatOutput result =
    let toStr p = 
        sprintf "%O:%O:%O" p.location.x p.location.y p.direction
        
    match result with
    | Ok p -> toStr p
    | Error p -> toStr p |> sprintf "O:%s"

let execute position obstacles commands =
    parseInput commands
    |> List.map toFunc
    |> List.fold (fun position command -> 
        position >>= move command obstacles) 
        (Ok position)
    |> formatOutput
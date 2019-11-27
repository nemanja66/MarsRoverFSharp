module Domain

type Result<'TSuccess, 'TFailure> =
    | Success of 'TSuccess
    | Failure of 'TFailure

let bind processFunc lastResult =
    match lastResult with
    | Success s -> processFunc s
    | Failure f -> Failure f

let (>>=) x f =
    bind f x

type Status =
    | Operational
    | Blocked

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

type Direction =
    | North
    | South
    | East
    | West

type Obstacle = {
    x: Coordinate
    y: Coordinate
} 

type Position = {
    x: Coordinate
    y: Coordinate
    direction: Direction
}

type Command =
    | RotateLeft
    | RotateRight
    | Move

let generateCoordinateSuccessor coordinate =
        match coordinate with
        | One -> Two
        | Two -> Three
        | Three -> Four
        | Four -> Five
        | Five -> Six
        | Six -> Seven
        | Seven -> Eight
        | Eight -> Nine
        | Nine -> Ten
        | Ten -> One

let generateCoordinatePredecessor coordinate =
        match coordinate with
        | Ten -> Nine
        | Nine -> Eight
        | Eight -> Seven
        | Seven -> Six
        | Six -> Five
        | Five -> Four
        | Four -> Three
        | Three -> Two
        | Two -> One
        | One -> Ten

let rotateLeft position = 
        match position.direction with
        | North -> {position with direction = West}
        | South -> {position with direction = East}
        | East  -> {position with direction = North}
        | West  -> {position with direction = South}

let rotateRight position = 
        match position.direction with
        | North -> {position with direction = East}
        | South -> {position with direction = West}
        | East  -> {position with direction = South}
        | West  -> {position with direction = North}

let calculateNewCoordinates position = 
        match position.direction with
        | North -> {position with y = generateCoordinateSuccessor position.y}
        | South -> {position with y = generateCoordinatePredecessor position.y}
        | East  -> {position with x = generateCoordinateSuccessor position.x}
        | West  -> {position with x = generateCoordinatePredecessor position.x}

let detectObstacle: Obstacle list -> Obstacle -> Obstacle option =
        fun obstacles maybeObstacle ->
            if List.contains maybeObstacle obstacles  then      
                {x=maybeObstacle.x; y=maybeObstacle.y} |> Some
                else None

let tryApplyCommand: Obstacle list -> Position -> Result<Position, Obstacle*Direction> =
    fun obstacles nextPosition ->
        match detectObstacle obstacles {x=nextPosition.x; y=nextPosition.y} with
            | Some obstacle -> Failure ({x=obstacle.x; y=obstacle.y}, nextPosition.direction)
            | None -> Success {x=nextPosition.x; y=nextPosition.y; direction = nextPosition.direction}
 
let move command obstacles position =
        match command with
            | RotateLeft -> position |> rotateLeft |> tryApplyCommand obstacles
            | RotateRight -> position |> rotateRight |> tryApplyCommand obstacles
            | Move -> position |> calculateNewCoordinates |> tryApplyCommand obstacles

let parseInput chars =
        let commands: Command list = List.Empty 
        Seq.toList chars |> List.fold (fun commands char ->
            match char with
                | 'L' -> Command.RotateLeft :: commands
                | 'R' -> Command.RotateRight :: commands
                | 'M' -> Command.Move :: commands
                |  _  -> commands
            ) commands |> List.rev

let DirectionToString direction =
         match direction with
         | North -> "N"
         | South -> "S"
         | East -> "E"
         | West -> "W"

let CoordinateToString coordinate =
        match coordinate with 
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

let formatOutput: Result<Position, Obstacle*Direction> -> string =
    fun result ->
        match result with
           | Success p -> CoordinateToString p.x + ":" + CoordinateToString p.y + ":" + DirectionToString p.direction
           | Failure o -> "O:" + ((o |> fst).x |> CoordinateToString) + ":" + ((o |> fst).y |> CoordinateToString) + ":" + (o |> snd |> DirectionToString)

let execute position obstacles commands =
        parseInput commands |> List.fold (fun position command -> position >>= move command obstacles) (Success position)
                            |> formatOutput

                            


        

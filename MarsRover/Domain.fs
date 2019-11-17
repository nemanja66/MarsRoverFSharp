module MarsRover

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

type Position = {
    x: Coordinate
    y: Coordinate
    direction: Direction
}

type Obstacle = {
    x: Coordinate
    y: Coordinate
}

type Rover = {
    Position: Position
    Status: Status
    DetectedObstacle: Option<Obstacle>
}

type Command =
    | RotateLeft
    | RotateRight
    | Move

let generateCoordinateSuccessor =
    fun coordinate ->
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

let generateCoordinatePredecessor =
    fun coordinate ->
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

let RotateLeft =
    fun position ->
        match position.direction with
        | North -> {position with direction = West}
        | South -> {position with direction = East}
        | East ->  {position with direction = North}
        | West ->  {position with direction = South}

let RotateRight =
    fun position ->
        match position.direction with
        | North -> {position with direction = East}
        | South -> {position with direction = West}
        | East ->  {position with direction = South}
        | West ->  {position with direction = North}

let CalculateNewCoordinates =
    fun position ->
        match position.direction with
        | North -> {position with y = generateCoordinateSuccessor position.y}
        | South -> {position with y = generateCoordinatePredecessor position.y}
        | East ->  {position with x = generateCoordinateSuccessor position.x}
        | West ->  {position with x = generateCoordinatePredecessor position.x}

let DetectCollision =
    fun obstacles maybeObstacle ->  
        if List.contains maybeObstacle obstacles  then      
            {x=maybeObstacle.x; y=maybeObstacle.y} |> Some
            else None

let TryApplyCommand =
    fun currentRover nextRover obstacles ->
        match DetectCollision obstacles {x=nextRover.Position.x; y=nextRover.Position.y} with
            | Some obstacle -> {currentRover with Status = Blocked; DetectedObstacle = Some obstacle}
            | None -> nextRover

let CalculateNewPosition =
    fun command rover obstacles ->
        match command with
            | RotateLeft -> TryApplyCommand rover {rover with Position = RotateLeft rover.Position} obstacles
            | RotateRight -> TryApplyCommand rover {rover with Position = RotateRight rover.Position} obstacles
            | Move -> TryApplyCommand rover {rover with Position = CalculateNewCoordinates rover.Position} obstacles

let ParseInput =
    fun chars ->
        let commands: Command list = List.Empty 
        Seq.toList chars |> List.fold (fun commands char ->
            match char with
                | 'L' -> Command.RotateLeft :: commands
                | 'R' -> Command.RotateRight :: commands
                | 'M' -> Command.Move :: commands
                |  _  -> commands
            ) commands |> List.rev

let getFirstElementOfAString: string -> char =
    fun input -> input.[0]

let DirectionToString =
     fun direction ->
         match direction with
         | North -> "N"
         | South -> "S"
         | East -> "E"
         | West -> "W"

let CoordinateToString = 
    fun coordinate ->
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
 
let FormatOutput: Rover -> string =
    fun rover -> 
        let position = CoordinateToString rover.Position.x + ":" + CoordinateToString rover.Position.y + ":" + DirectionToString rover.Position.direction
        match rover.DetectedObstacle with
           | Some obstacle -> position + " " + "O:" + CoordinateToString obstacle.x + ":" + CoordinateToString obstacle.y
           | None -> position
                        
let Execute =
    fun rover obstacles commands ->
        ParseInput commands |> List.fold (fun rover command ->
        match rover.Status with
        | Operational -> CalculateNewPosition command rover obstacles
        | Blocked -> {rover with Position = rover.Position}) rover
        |> FormatOutput

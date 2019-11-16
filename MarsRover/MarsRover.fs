﻿module MarsRover

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

type Rotate = Position -> Position

let generateCoordinateSuccessor: Coordinate -> Coordinate =
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

let generateCoordinatePredecessor: Coordinate -> Coordinate =
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

let RotateLeft: Rotate =
    fun position ->
        match position.direction with
        | North -> {position with direction = West}
        | South -> {position with direction = East}
        | East ->  {position with direction = North}
        | West ->  {position with direction = South}

let RotateRight: Rotate =
    fun position ->
        match position.direction with
        | North -> {x = position.x; y = position.y; direction = East}
        | South -> {x = position.x; y = position.y; direction = West}
        | East ->  {x = position.x; y = position.y; direction = South}
        | West ->  {x = position.x; y = position.y; direction = North}

let Move: Position -> Position =
    fun position ->
        match position.direction with
        | North -> {position with y = generateCoordinateSuccessor position.y}
        | South -> {position with y = generateCoordinatePredecessor position.y}
        | East ->  {position with x = generateCoordinateSuccessor position.x}
        | West ->  {position with x = generateCoordinatePredecessor position.x}

let detectCollision: Obstacle list -> Position -> Option<Obstacle> =
    fun obstacles position ->  
        if List.contains {x=position.x; y=position.y} obstacles  then      
            {x=position.x; y=position.y} |> Some
            else None

let tryApplyCommand: Rover -> Rover -> Obstacle list -> Rover =
    fun currentRover nextRover obstacles ->
      //match detectCollision obstacles nextRover.Position with
        detectCollision obstacles nextRover.Position |> Option.fold (fun _ obstacle -> {currentRover with Status = Blocked; DetectedObstacle = Some obstacle}) nextRover
        //| Some o -> {nextRover with Status = Blocked}
        //| None -> nextRover

let generatePosition: char -> Rover -> Obstacle list -> Rover =
    fun command rover obstacles ->
        match command with
            | 'L' -> tryApplyCommand rover {rover with Position = RotateLeft rover.Position} obstacles
            | 'R' -> tryApplyCommand rover {rover with Position = RotateRight rover.Position} obstacles
            | 'M' -> tryApplyCommand rover {rover with Position = Move rover.Position} obstacles
            |  _  -> {rover with Position = rover.Position}

let Execute: string -> Rover -> Obstacle list -> Rover =
    fun commands rover obstacles ->
        Seq.toList commands |> List.fold (fun rover command ->
        match rover.Status with
        | Operational -> generatePosition command rover obstacles
        | Blocked -> {rover with Position = rover.Position}) rover

module MarsRoverShould

open Xunit
open Domain

[<Fact>]
let ``MakeNoMovementWhenPassedEmptyArrayOfCommands`` () =
    let commands = ""
    let startingPosition: Position = {x = Coordinate.One; y = Coordinate.One; direction = Direction.North}
    let startingRover: Rover = {Position = startingPosition; Status = Operational; DetectedObstacle = None}
    let obstacles: Obstacle list = List.empty<Obstacle>
    let result = Execute startingRover obstacles commands
    let expectedResult = "1:1:N"
    let x = expectedResult = result
    Assert.True(x)

[<Theory>]
[<InlineData("R", "1:1:E")>]
[<InlineData("RR", "1:1:S")>]
[<InlineData("RRR", "1:1:W")>]
[<InlineData("RRRR", "1:1:N")>]
let ``RotateRight`` commands expectedResult =
    let startingPosition: Position = {x = Coordinate.One; y = Coordinate.One; direction = Direction.North}
    let rover: Rover = {Position = startingPosition; Status = Operational; DetectedObstacle = None}
    let obstacles: Obstacle list = List.empty<Obstacle>
    let result = Execute rover obstacles commands
    let x = result = expectedResult
    Assert.True(x)

[<Theory>]
[<InlineData("L", "1:1:W")>]
[<InlineData("LL", "1:1:S")>]
[<InlineData("LLL", "1:1:E")>]
[<InlineData("LLLL", "1:1:N")>]
let ``RotateLeft`` commands expectedResult =
    let startingPosition: Position = {x = Coordinate.One; y = Coordinate.One; direction = Direction.North}
    let rover: Rover = {Position = startingPosition; Status = Operational; DetectedObstacle = None}
    let obstacles: Obstacle list = List.empty<Obstacle>
    let result = Execute rover obstacles commands
    let x = result = expectedResult
    Assert.True(x)

[<Theory>]
[<InlineData("M", "1:2:N")>]
[<InlineData("MM", "1:3:N")>]
let ``MoveNorth`` commands expectedResult =
    let startingPosition: Position = {x = Coordinate.One; y = Coordinate.One; direction = Direction.North}
    let rover: Rover = {Position = startingPosition; Status = Operational; DetectedObstacle = None}
    let obstacles: Obstacle list = List.empty<Obstacle>
    let result = Execute rover obstacles commands
    let x = result = expectedResult
    Assert.True(x)

[<Theory>]
[<InlineData("M", "1:2:S")>]
[<InlineData("MM", "1:1:S")>]
let ``MoveSouth`` commands expectedResult =
    let startingPosition: Position = {x = Coordinate.One; y = Coordinate.Three; direction = Direction.South}
    let rover: Rover = {Position = startingPosition; Status = Operational; DetectedObstacle = None}
    let obstacles: Obstacle list = List.empty<Obstacle>
    let result = Execute rover obstacles commands
    let x = result = expectedResult
    Assert.True(x)

[<Theory>]
[<InlineData("M", "2:1:E")>]
[<InlineData("MM", "3:1:E")>]
let ``MoveEast`` commands expectedResult =
    let startingPosition: Position = {x = Coordinate.One; y = Coordinate.One; direction = Direction.East}
    let rover: Rover = {Position = startingPosition; Status = Operational; DetectedObstacle = None}
    let obstacles: Obstacle list = List.empty<Obstacle>
    let result = Execute rover obstacles commands
    let x = result = expectedResult
    Assert.True(x)

[<Theory>]
[<InlineData("M", "2:1:W")>]
[<InlineData("MM", "1:1:W")>]
let ``MoveWest`` commands expectedResult =
    let startingPosition: Position = {x = Coordinate.Three; y = Coordinate.One; direction = Direction.West}
    let rover: Rover = {Position = startingPosition; Status = Operational; DetectedObstacle = None}
    let obstacles: Obstacle list = List.empty<Obstacle>
    let result = Execute rover obstacles commands
    let x = result = expectedResult
    Assert.True(x)

[<Fact>]
let ``GoToPosition23E`` () =
    let commands = "MMRM"
    let startingPosition: Position = {x = Coordinate.One; y = Coordinate.One; direction = Direction.North}
    let rover: Rover = {Position = startingPosition; Status = Operational; DetectedObstacle = None}
    let obstacles: Obstacle list = List.empty<Obstacle>
    let result = Execute rover obstacles commands
    let expectedResult = "2:3:E"
    let x = result = expectedResult
    Assert.True(x)

[<Theory>]
[<InlineData("LLMMMM")>]
let ``GoToPositionFrom23ETo83W`` (commands) =
    let startingPosition: Position = {x = Coordinate.Two; y = Coordinate.Three; direction = Direction.East}
    let rover: Rover = {Position = startingPosition; Status = Operational; DetectedObstacle = None}
    let obstacles: Obstacle list = List.empty<Obstacle>
    let result = Execute rover obstacles commands
    let expectedResult = "8:3:W"
    let x = expectedResult = result
    Assert.True(x)

[<Fact>]
let ``HitAObstacleStopAndReturnPositionOfTheObstacle`` () =
     let commands = "MMRMMRMM"
     let obstacles: Obstacle list = [{x=Three; y=Three}]
     let startingPosition: Position = {x = Coordinate.One; y = Coordinate.One; direction = Direction.North}
     let startingRover: Rover = {Position = startingPosition; Status = Operational; DetectedObstacle = None}
     let expectedResult = "2:3:E O:3:3"
     let result = Execute startingRover obstacles commands
     let x = result = expectedResult
     Assert.True(x)

[<Fact>]
let ``HitAObstacleStopAndReturnPositionOfTheObstacleOnTheFirstMove`` () =
     let commands = "MMM"
     let obstacles: Obstacle list = [{x=One; y=Two}]
     let startingPosition: Position = {x = Coordinate.One; y = Coordinate.One; direction = Direction.North}
     let startingRover: Rover = {Position = startingPosition; Status = Operational; DetectedObstacle = None}
     let expectedResult = "1:1:N O:1:2"
     let result = Execute startingRover obstacles commands
     let x = expectedResult = result
     Assert.True(x)
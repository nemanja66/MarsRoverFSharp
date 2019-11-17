module Tests

open System
open Xunit
open MarsRover

[<Fact>]
let ``GoToPosition23E`` () =
    let commands = "MMRM"
    let startingPosition: Position = {x = Coordinate.One; y = Coordinate.One; direction = Direction.North}
    let rover: Rover = {Position = startingPosition; Status = MarsRover.Operational; DetectedObstacle = None}
    let obstacles: Obstacle list = List.empty<MarsRover.Obstacle>
    let result = Execute rover obstacles commands
    let expectedResult = "2:3:E"
    let x = result = expectedResult
    Assert.True(x)

[<Fact>]
let ``GoToPositionFrom23ETo83W`` () =
    let commands = "LLMMMM"
    let startingPosition: MarsRover.Position = {x = MarsRover.Coordinate.Two; y = MarsRover.Coordinate.Three; direction = MarsRover.Direction.East}
    let rover: MarsRover.Rover = {Position = startingPosition; Status = MarsRover.Operational; DetectedObstacle = None}
    let obstacles: MarsRover.Obstacle list = List.empty<MarsRover.Obstacle>
    let result = MarsRover.Execute rover obstacles commands
    let expectedResult = "8:3:W"
    let x = expectedResult = result
    Assert.True(x)

[<Fact>]
let ``HitAObstacleStopAndReturnPositionOfTheObstacle`` () =
     let commands = "MMRMMRMM"
     let obstacles: MarsRover.Obstacle list = [{x=MarsRover.Three; y=MarsRover.Three}]
     let startingPosition: MarsRover.Position = {x = MarsRover.Coordinate.One; y = MarsRover.Coordinate.One; direction = MarsRover.Direction.North}
     let startingRover: MarsRover.Rover = {Position = startingPosition; Status = MarsRover.Operational; DetectedObstacle = None}
     let expectedResult = "2:3:E O:3:3"
     let result = MarsRover.Execute startingRover obstacles commands
     let x = result = expectedResult
     Assert.True(x)

[<Fact>]
let ``HitAObstacleStopAndReturnPositionOfTheObstacleOnTheFirstMove`` () =
     let commands = "MMM"
     let obstacles: MarsRover.Obstacle list = [{x=MarsRover.One; y=MarsRover.Two}]
     let startingPosition: MarsRover.Position = {x = MarsRover.Coordinate.One; y = MarsRover.Coordinate.One; direction = MarsRover.Direction.North}
     let startingRover: MarsRover.Rover = {Position = startingPosition; Status = MarsRover.Operational; DetectedObstacle = None}
     let expectedResult = "1:1:N O:1:2"
     let result = MarsRover.Execute startingRover obstacles commands
     let x = expectedResult = result
     Assert.True(x)

[<Fact>]
let ``MakeNoMovementWhenPassedEmptyArrayOfCommands`` () =
    let commands = ""
    let startingPosition: MarsRover.Position = {x = MarsRover.Coordinate.One; y = MarsRover.Coordinate.One; direction = MarsRover.Direction.North}
    let startingRover: MarsRover.Rover = {Position = startingPosition; Status = MarsRover.Operational; DetectedObstacle = None}
    let obstacles: MarsRover.Obstacle list = List.empty<MarsRover.Obstacle>
    let result = MarsRover.Execute startingRover obstacles commands
    let expectedResult = "1:1:N"
    let x = expectedResult = result
    Assert.True(x)
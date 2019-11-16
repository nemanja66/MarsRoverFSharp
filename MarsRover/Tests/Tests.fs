module Tests

open System
open Xunit

[<Fact>]
let ``GoToPosition23E`` () =
    let commands = "MMRM"
    let startingPosition: MarsRover.Position = {x = MarsRover.Coordinate.One; y = MarsRover.Coordinate.One; direction = MarsRover.Direction.North}
    let expectedPosition: MarsRover.Position = {x = MarsRover.Coordinate.Two; y = MarsRover.Coordinate.Three; direction = MarsRover.Direction.East}
    let startingRover: MarsRover.Rover = {Position = startingPosition; Status = MarsRover.Operational; DetectedObstacle = None}
    let expectedRover: MarsRover.Rover = {Position = expectedPosition; Status = MarsRover.Operational; DetectedObstacle = None}
    let obstacles: MarsRover.Obstacle list = List.empty<MarsRover.Obstacle>
    let resultRover = MarsRover.Execute commands startingRover obstacles
    let x = expectedRover = resultRover
    Assert.True(x)

[<Fact>]
let ``GoToPositionFrom23ETo83W`` () =
    let commands = "LLMMMM"
    let startingPosition: MarsRover.Position = {x = MarsRover.Coordinate.Two; y = MarsRover.Coordinate.Three; direction = MarsRover.Direction.East}
    let expectedPosition: MarsRover.Position = {x = MarsRover.Coordinate.Eight; y = MarsRover.Coordinate.Three; direction = MarsRover.Direction.West}
    let startingRover: MarsRover.Rover = {Position = startingPosition; Status = MarsRover.Operational; DetectedObstacle = None}
    let expectedRover: MarsRover.Rover = {Position = expectedPosition; Status = MarsRover.Operational; DetectedObstacle = None}
    let obstacles: MarsRover.Obstacle list = List.empty<MarsRover.Obstacle>
    let resultRover = MarsRover.Execute commands startingRover obstacles
    let x = expectedRover = resultRover
    Assert.True(x)

[<Fact>]
let ``HitAObstacleStopAndReturnPositionOfTheObstacle`` () =
     let commands = "MMRMMRMM"
     let obstacles: MarsRover.Obstacle list = [{x=MarsRover.Three; y=MarsRover.Three}]
     let startingPosition: MarsRover.Position = {x = MarsRover.Coordinate.One; y = MarsRover.Coordinate.One; direction = MarsRover.Direction.North}
     let expectedPosition: MarsRover.Position = {x = MarsRover.Coordinate.Two; y = MarsRover.Coordinate.Three; direction = MarsRover.Direction.East}
     let startingRover: MarsRover.Rover = {Position = startingPosition; Status = MarsRover.Operational; DetectedObstacle = None}
     let expectedRover: MarsRover.Rover = {Position = expectedPosition; Status = MarsRover.Blocked; DetectedObstacle = Some {x=MarsRover.Three; y=MarsRover.Three}}
     let resultRover = MarsRover.Execute commands startingRover obstacles
     let x = expectedRover = resultRover
     Assert.True(x)

[<Fact>]
let ``HitAObstacleStopAndReturnPositionOfTheObstacleAnotherWay`` () =
     let commands = "RMMLMMRMM"
     let obstacles: MarsRover.Obstacle list = [{x=MarsRover.Three; y=MarsRover.Three}]
     let startingPosition: MarsRover.Position = {x = MarsRover.Coordinate.One; y = MarsRover.Coordinate.One; direction = MarsRover.Direction.North}
     let expectedPosition: MarsRover.Position = {x = MarsRover.Coordinate.Three; y = MarsRover.Coordinate.Two; direction = MarsRover.Direction.North}
     let startingRover: MarsRover.Rover = {Position = startingPosition; Status = MarsRover.Operational; DetectedObstacle = None}
     let expectedRover: MarsRover.Rover = {Position = expectedPosition; Status = MarsRover.Blocked; DetectedObstacle = Some {x=MarsRover.Three; y=MarsRover.Three}}
     let resultRover = MarsRover.Execute commands startingRover obstacles
     let x = expectedRover = resultRover
     Assert.True(x)

[<Fact>]
let ``HitAObstacleStopAndReturnPositionOfTheObstacleOnTheFirstMove`` () =
     let commands = "MMM"
     let obstacles: MarsRover.Obstacle list = [{x=MarsRover.One; y=MarsRover.Two}]
     let startingPosition: MarsRover.Position = {x = MarsRover.Coordinate.One; y = MarsRover.Coordinate.One; direction = MarsRover.Direction.North}
     let expectedPosition: MarsRover.Position = {x = MarsRover.Coordinate.One; y = MarsRover.Coordinate.One; direction = MarsRover.Direction.North}
     let startingRover: MarsRover.Rover = {Position = startingPosition; Status = MarsRover.Operational; DetectedObstacle = None}
     let expectedRover: MarsRover.Rover = {Position = expectedPosition; Status = MarsRover.Blocked; DetectedObstacle = Some {x=MarsRover.One; y=MarsRover.Two}}
     let resultRover = MarsRover.Execute commands startingRover obstacles
     let x = expectedRover = resultRover
     Assert.True(x)
module API

open MarsRover

// Dirty module that pretends to implement real i.e. filesystem operations
module Impure = 
    //Gets the persisted position of a rover and maps i to the domain object
    let GetRoverData path = {Position = {x = Coordinate.One; y = Coordinate.One; direction = Direction.North}; Status = Operational; DetectedObstacle = None} 
    let SaveResult roverData path = "" //Parses and saves theoutput of a rover movement
    let GetObstacles path = List.empty<Obstacle> //Gets the obstacles

 // More pretension
let path = ""

//let Execute: string -> string = 
//    fun commands -> 
//        let rover = Impure.GetRoverData path 
//        let obstacles = Impure.GetObstacles path
//        let result = Execute rover obstacles commands
//        Impure.SaveResult result path
//        result

let Execute2: string -> string = 
    fun commands -> 
      Impure.GetRoverData path 
          |> MarsRover.Execute
          |> fun execute -> execute (Impure.GetObstacles path)
          |> fun execute -> execute commands
          |> Impure.SaveResult path


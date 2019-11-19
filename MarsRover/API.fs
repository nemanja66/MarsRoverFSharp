module API

open Domain

// This module is created in order to make things more interesting. When the rover receives a set of commands, 
// it goes to a specific location. When it receives the next set of commands, we want for the rover to continue the movement 
// from the previous location. This is the reason why we need some kind of persistence.
// Also, we need to get obstacles from somewhere.


// Dirty module that pretends to implement real, for example, filesystem operations
module Impure = 
    //Gets the persisted position of a rover and maps i to the domain object
    let GetRoverData path = {Position = {x = Coordinate.One; y = Coordinate.One; direction = Direction.North}; Status = Operational; DetectedObstacle = None} 
    let SaveResult roverData path = () //Parses and saves the output of a rover movement
    let GetObstacles path = List.empty<Obstacle> //Gets the obstacles

 // More pretension
let path = ""

// The IMPURE-PURE-IMPURE sandwich
//THIS IS THE API FOR THE USER
let Execute commands = 
        let rover = Impure.GetRoverData path 
        let obstacles = Impure.GetObstacles path
        let result = Execute rover obstacles commands //Pure
        Impure.SaveResult result path
        result

//Execute with pipe attempt

//let SaveResult roverData path = "" //Parses and saves the output of a rover movement
//let Execute: string -> string = 
//    fun commands -> 
//      Impure.GetRoverData path 
//          |> MarsRover.Execute
//          |> fun execute -> execute (Impure.GetObstacles path)
//          |> fun execute -> execute commands
//          |> Impure.SaveResult path


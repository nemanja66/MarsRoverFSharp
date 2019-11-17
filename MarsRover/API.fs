module API

open MarsRover

// Dirty module that pretends to implement real i.e. filesystem operations
module Impure = 
    //Gets the persisted position of a rover and maps i to the domain object
    let GetPosition path = {x = MarsRover.Coordinate.One; y = MarsRover.Coordinate.One; direction = MarsRover.Direction.North} 
    let SaveResult path rover = () //Parses and saves theoutput of a rover movement
    let GetObstacles path = List.empty<Obstacle> //Gets the obstacles

 // More pretension
let path = ""

let Execute: string -> string = 
    fun commands -> 
        let position = Impure.GetPosition path 
        let obstacles = Impure.GetObstacles path
        let rover =  {Position = position; Status = Operational; DetectedObstacle = None}
        let result = MarsRover.Execute rover obstacles commands
        Impure.SaveResult path result
        result




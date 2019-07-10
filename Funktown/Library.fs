namespace Funktown


open Microsoft.Azure.WebJobs
open Microsoft.AspNetCore.Mvc
open Microsoft.Azure.WebJobs.Extensions.Http
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging
open Sudoku

module AModule =

    [<FunctionName("Sudoku")>]
    let public Run([<HttpTrigger(AuthorizationLevel.Function, "get", Route = null)>]  req: HttpRequest, log: ILogger) =
        let success, puzzle = req.GetQueryParameterDictionary().TryGetValue "puzzle"
        if success then 
            let grid = parseGrid puzzle
            match grid with 
            | Some m -> 
                let solved, count = solve m
                OkObjectResult(renderGrid solved)
            | None -> OkObjectResult(sprintf "Couldn't parse puzzle %s" puzzle)
        else OkObjectResult("Puzzle is invalid")
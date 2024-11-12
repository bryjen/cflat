namespace CFlat.Repl

// For more information see https://aka.ms/fsharp-console-apps

module Program =
    
    [<EntryPoint>]
    let main argv =
        
        let something = 1
        if something > 10 then
            ()
            
        printfn "Hello from F#"
        0
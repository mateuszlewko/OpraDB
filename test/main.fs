open Expecto

[<EntryPoint>]
let main argv =
    runTestsInAssembly { defaultConfig with mySpiritIsWeak = true } argv

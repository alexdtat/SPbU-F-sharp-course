module hw4Task3.Task3

open System
open System.IO
open hw4.PhoneBook

let printCommands =
    printfn
        "Available commands:
    1 -- add contact (name and number);
    2 -- find number by name;
    3 -- find name by number;
    4 -- print current content of the phone book;
    5 -- save the phone book into the file (with given path);
    6 -- load the phone book from the file (with given path);
    0 -- exit."

let getName () =
    printfn "Please, enter the name:"
    Console.ReadLine()

let rec getNumber () =
    printfn "Please, enter the number:"

    try
        Console.ReadLine() |> uint64
    with :? FormatException ->
        printfn "Incorrect number format. Please, try again:"
        getNumber ()

let rec getSavePath () =
    printfn "Please, enter the save file's directory path:"
    let path = Console.ReadLine()

    match (Directory.Exists path) with
    | true -> path
    | false ->
        printfn "Directory does not exist. Please, try again:"
        getSavePath ()

let rec getLoadPath () =
    printfn "Please, enter the save file's directory path:"
    let path = Console.ReadLine()

    match (File.Exists path) with
    | true -> path
    | false ->
        printfn "File does not exist. Please, try again:"
        getSavePath ()

let processCLI =
    let rec command phoneBook =
        printfn "Please, enter a command:"
        let input = Console.ReadLine()

        match input with
        | "0" -> ()
        | "1" -> (getName (), getNumber (), phoneBook) |||> addContact |> command
        | "2" ->
            let number = (getName (), phoneBook) ||> findNumberByName

            printfn "%A"
            <| match number with
               | Some num -> num |> string
               | None -> "Nothing was found."

            command phoneBook
        | "3" ->
            let name = (getNumber (), phoneBook) ||> findNameByNumber

            printfn "%A"
            <| match name with
               | Some name -> name
               | None -> "Nothing was found."
            command phoneBook
        | "4" ->
            printPhoneBook phoneBook
            command phoneBook
        | "5" ->
            (getSavePath (), phoneBook) ||> writeIntoFile
            command phoneBook
        | "6" ->
            let path = getLoadPath ()

            try
                loadFromFile path |> command
            with
            | :? FileLoadException
            | :? FormatException ->
                printfn
                    "Invalid file or incorrect format. Contacts must be written like:
                {name}: {number}
                Please, try again:"

                command phoneBook
        | _ ->
            printfn "Unknown command."
            printCommands
            command phoneBook

    printCommands
    command List.Empty

processCLI

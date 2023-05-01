module hw4.PhoneBook

open System.IO


type phoneContact = { name: string; phoneNumber: uint64 }
type phoneBook = phoneContact List

let addContact name number (phoneBook: phoneBook) =
    { name = name; phoneNumber = number } :: phoneBook

let findNumberByName name (phoneBook: phoneBook) =
    phoneBook
    |> List.tryFind (fun contact -> contact.name = name)
    |> Option.map (fun contact -> contact.phoneNumber)

let findNameByNumber number (phoneBook: phoneBook) =
    phoneBook
    |> List.tryFind (fun contact -> contact.phoneNumber = number)
    |> Option.map (fun contact -> contact.name)

let writeIntoFile (filePath: string) (phoneBook: phoneBook) =
    use streamWriter = new StreamWriter(filePath)

    phoneBook
    |> List.rev
    |> List.iter (fun contact -> streamWriter.WriteLine($"{contact.name}: {contact.phoneNumber}"))

let loadFromFile (filePath: string) =
    use streamReader = new StreamReader(filePath)

    let rec recReadLine phoneBook =
        let line = streamReader.ReadLine()

        match line with
        | null -> phoneBook
        | line ->
            let split = line.Split ": "

            match split with
            | _ when split.Length <> 2 -> raise (FileLoadException())
            | _ -> (split[0], split[1] |> uint64, phoneBook) |||> addContact |> recReadLine

    List.Empty |> recReadLine

let printPhoneBook (phoneBook: phoneBook) =
    phoneBook
    |> List.iter (fun contact -> printfn $"Name: {contact.name}\tPhone number: {contact.phoneNumber}")

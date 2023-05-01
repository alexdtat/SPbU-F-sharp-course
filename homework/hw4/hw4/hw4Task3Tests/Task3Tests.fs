module hw4Task3Tests

open System.IO
open NUnit.Framework
open hw4.PhoneBook
open FsUnit

let testedPhoneBook =
    []
    |> addContact "Harrier du Bois" 987654321UL
    |> addContact "Yang Wenli" 123456789UL

[<Test>]
let testAddContact () =
    testedPhoneBook
    |> addContact "Alexey" 244UL
    |> should
        equal
        ([ { name = "Harrier du Bois"
             phoneNumber = 987654321UL }
           { name = "Yang Wenli"
             phoneNumber = 123456789UL }
           { name = "Alexey"; phoneNumber = 244UL } ] |> List.rev)

[<Test>]
let testFindNumberByName () =
    ("Yang Wenli", testedPhoneBook) ||> findNumberByName |> should equal (Some 123456789UL)
    
[<Test>]
let testFindNameByNumber () =
    (987654321UL, testedPhoneBook) ||> findNameByNumber |> should equal (Some "Harrier du Bois")

[<Test>]
let writeReadTest () =
    let name = "savedPhoneBook.txt"
    (name, testedPhoneBook) ||> writeIntoFile
    name |> loadFromFile |> should equal testedPhoneBook
    File.Delete(name)

module Security

module MD5 =    
    open System
    open System.Security.Cryptography
    open System.Text

    let encrypt (plainText:string) =         
        use hasher = MD5.Create()
        let encrypted = 
            plainText 
            |> Encoding.Default.GetBytes
            |> hasher.ComputeHash
            |> Seq.fold (fun sb byte -> Printf.bprintf sb "%x2" byte; sb) (new StringBuilder())
        encrypted.ToString()



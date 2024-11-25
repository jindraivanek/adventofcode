open System.Net.Http
let baseUrl = "https://adventofcode.com"
let startYear = 2022
let client = new HttpClient()
let cookie = System.IO.File.ReadAllText("cookie.txt")

let getInputData (year: int) (day: int) =
    let inputDataPath = $"input/%04i{year}/%02i{day}.txt"
    printfn $"Input data path: {inputDataPath}"
    if not <| System.IO.File.Exists(inputDataPath) then
        let url = $"{baseUrl}/{year}/day/{day}/input"
        printfn $"Downloading input data from {url}" 
        let request = new HttpRequestMessage(HttpMethod.Get, url)
        request.Headers.Add("Cookie", cookie)
        let response = client.Send(request)
        let data = response.Content.ReadAsStringAsync().Result
        System.IO.Directory.CreateDirectory(System.IO.Path.GetDirectoryName(inputDataPath)) |> ignore
        System.IO.File.WriteAllText(inputDataPath, data)

let getAllData () =
    let now = System.DateTime.Now.Date
    for year in startYear..now.Year do
        for day in 1..25 do
            if System.DateTime(year, 12, day) <= now then
                getInputData year day

getAllData()
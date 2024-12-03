let templateSource = System.IO.File.ReadAllText("Template.fs")
for i in 1..25 do
    let fileName = $"%02i{i}.fs"
    let template = templateSource.Replace("day01", $"day%02i{i}").Replace("Day = 1", $"Day = %i{i}")
    if not <| System.IO.File.Exists(fileName) then
        System.IO.File.WriteAllText(fileName, template)
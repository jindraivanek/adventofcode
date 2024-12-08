namespace fs

open Godot
        
type MainFs(this: Node2D) =
    let size = 32
    let moveSpeed = 1000.f
    let zoomSpeed = 0.1f

    let loadRes =
        lazy
        {|
          tileMap = this.GetNode<TileMapLayer>("TileMap")
          camera = this.GetNode<Camera2D>("Player/Camera2D")
          player = this.GetNode<CharacterBody2D>("Player")
        |}
    
    let getModulatedSource srcId textureImg color = 
        lazy
        let res = loadRes.Value
        let tlsrc = new TileSetAtlasSource()
        let txt = ImageTexture.CreateFromImage(Image.LoadFromFile(textureImg))
        tlsrc.Texture <- txt
        let newSrc = res.tileMap.TileSet.AddSource(res.tileMap.TileSet.GetSource(srcId).Duplicate() :?> TileSetAtlasSource)
        let src = res.tileMap.TileSet.GetSource(newSrc) :?> TileSetAtlasSource
        src.CreateTile(Vector2I(0, 0))
        let tile = src.GetTileData(Vector2I(0, 0), 0)
        tile.Modulate <- color
        newSrc
    
    let wall = getModulatedSource 1 "res://assets/player.png" (Color(1.f, 1.f, 1.f, 1.f))
    let player = getModulatedSource 1 "res://assets/player.png" (Color(0.f, 1.f, 0f, 0.5f))
    let peekHigh = getModulatedSource 1 "res://assets/player.png" (Color(0.5f, 0.f, 0.5f, 0.5f))
    let peek = getModulatedSource 1 "res://assets/player.png" (Color(0.5f, 0.5f, 0.5f, 0.5f))
    let trail = getModulatedSource 1 "res://assets/player.png" (Color(0.f, 0.f, 0.5f, 0.5f))

    let drawStep step =
        let res = loadRes.Value
        let grid = fs.day06.sol[step % fs.day06.sol.Length] 
        res.player.Position <- Vector2((float32 (fst grid.Size)) / 2.f * 16.f, (float32 (snd grid.Size)) / 2.f * 16.f)
        res.tileMap.Clear()
        grid.Grid |> Map.iter (fun (i, j) ->
            function
            | Grid.Floor -> res.tileMap.SetCell(Vector2I(i, j), -1)
            | Grid.Wall -> res.tileMap.SetCell(Vector2I(i, j), wall.Value, Vector2I(0, 0))
            | Grid.Player _ -> 
                if s.Guard = s.Start then
                    res.player.Position <- Vector2((float32 i) * 16.f, (float32 j) * 16.f)
                res.tileMap.SetCell(Vector2I(i, j), player.Value, Vector2I(0, 0))
            | Grid.Peek -> res.tileMap.SetCell(Vector2I(i, j), peek.Value, Vector2I(0, 0))
            | Grid.PeekHighlight -> res.tileMap.SetCell(Vector2I(i, j), peekHigh.Value, Vector2I(0, 0))
            | Grid.Trail -> res.tileMap.SetCell(Vector2I(i, j), trail.Value, Vector2I(0, 0))
            )

    let time = System.Diagnostics.Stopwatch.StartNew()
    let sol = fs.day06.sol |> Seq.toArray
    let mutable step = sol.Length - 1
    
    member _.ready() = 
       drawStep step
       time.Restart()
        
    member _.process() = 
        let res = loadRes.Value
        let moveDir = Input.GetVector("left", "right", "up", "down")
        res.player.Velocity <- moveDir * moveSpeed
        res.player.MoveAndSlide()

        let step = time.Elapsed.TotalSeconds * 20. |> int
        drawStep step

    member _.input(event: InputEvent) =
        let res = loadRes.Value
        match event with
        | :? InputEventMouseButton as e ->
            //GD.Print(e.AsText())
            match e.ButtonIndex with
            | MouseButton.WheelUp -> res.camera.Zoom <- res.camera.Zoom - zoomSpeed * Vector2.One
            | MouseButton.WheelDown -> res.camera.Zoom <- res.camera.Zoom + zoomSpeed * Vector2.One
            | _ -> ()
        | :? InputEventKey as e when e.Pressed ->
            //GD.Print(e.AsText())
            match e.Keycode with
            | Key.Up -> step <- step + 1
            | Key.Down -> step <- step - 1
            | _ -> ()
        | _ -> ()
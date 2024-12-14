using Godot;
using System;
using fs;

public partial class Main : Node2D
{
	private MainFs mainFs;

	public Main()
	{
		mainFs = new MainFs(this);
	}
	// Called when the node enters the scene tree for the first time.
	public override void _Ready()
	{
		GD.Print("SimplePrintCs: C# Running...");
		mainFs.ready();
	}

	// Called every frame. 'delta' is the elapsed time since the previous frame.
	public override void _Process(double delta)
	{
		mainFs.process(delta);
	}

	public override void _UnhandledInput(InputEvent inputEvent)
	{
		mainFs.input(inputEvent);
	}
}

{ Game initialization.
	This unit is cross-platform.
	It will be used by the platform-specific program or library file.

	Feel free to use this code as a starting point for your own projects.
	(This code is in public domain, unlike most other CGE code which
	is covered by the LGPL license variant, see the COPYING.txt file.) }
unit GameInitialize;

interface

implementation

uses SysUtils,
	CastleWindow, CastleLog,
	GameLore, GameMaps, GameTranslations
	{$region 'Castle Initialization Uses'}
	// The content here may be automatically updated by CGE editor.
	, GameViewLogin, GameViewCharacterList
	, GameViewLoading
	, GameViewPlay
	{$endregion 'Castle Initialization Uses'};

var
	Window: TCastleWindow;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
	{ Adjust container settings for a scalable UI (adjusts to any window size in a smart way). }
	Window.Container.LoadSettings('castle-data:/CastleSettings.xml');

	LoreCollection.Initialize;
	MapIndex.Initialize;

	GlobalTranslations := TGameMOFile.Create;
	TranslateAllGameDesigns;

	{$region 'Castle State Creation'}
	// The content here may be automatically updated by CGE editor.
	ViewLogin := TViewLogin.Create(Application);
	ViewCharacterList := TViewCharacterList.Create(Application);
	ViewLoading := TViewLoading.Create(Application);
	ViewPlay := TViewPlay.Create(Application);
	{$endregion 'Castle State Creation'}

	Window.Container.View := ViewLogin;
end;

initialization
	{ Initialize Application.OnInitialize. }
	Application.OnInitialize := @ApplicationInitialize;

	{ Create and assign Application.MainWindow. }
	Window := TCastleWindow.Create(Application);
	Window.ParseParameters; // allows to control window size / fullscreen on the command-line
	Application.MainWindow := Window;

	{ You should not need to do *anything* more in the unit "initialization" section.
		Most of your game initialization should happen inside ApplicationInitialize.
		In particular, it is not allowed to read files before ApplicationInitialize
		(because in case of non-desktop platforms,
		some necessary resources may not be prepared yet). }
end.


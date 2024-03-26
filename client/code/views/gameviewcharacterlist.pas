unit GameViewCharacterList;

interface

uses SysUtils, Classes,
	CastleVectors, CastleComponentSerialize,
	CastleUIControls, CastleControls, CastleKeysMouse,
	CastleFonts, CastleStringUtils, CastleUnicode,
	GameTypes, GameLog,
	GameUIComponents,
	GameLore,
	GameNetwork,
	GameModels, GameModels.General, GameModels.CharacterList, GameModels.EnterGame;

type

	TCharacterSelection = class(TCastleDesign)
	private
		FId: TUlid;

	public
		property Id: TUlid read FId write FId;
	end;


	TViewCharacterList = class(TCastleView)
	published
		CharacterList: TCastleVerticalGroup;
		LogoutButton: TGameButton;

	private
		FPlayerId: TUlid;

	public
		constructor Create(AOwner: TComponent); override;
		procedure Start; override;
		procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
		function Press(const Event: TInputPressRelease): Boolean; override;

		procedure DoLogout(Sender: TObject);

		procedure DoLoadCharacterList();
		procedure OnCharacterList(const ACharacterList: TModelBase);

		procedure DoEnterGame(const Ui: TCastleUserInterface; const Event: TInputPressRelease; var Handled: Boolean);
		procedure OnEnterGame(const Success: TModelBase);

	end;

var
	ViewCharacterList: TViewCharacterList;

implementation

uses GameViewLoading;

{ TViewCharacterList ----------------------------------------------------------------- }

constructor TViewCharacterList.Create(AOwner: TComponent);
begin
	inherited;
	DesignUrl := 'castle-data:/gameviewcharacterlist.castle-user-interface';
end;

procedure TViewCharacterList.Start;
begin
	inherited;
	GlobalClient.ContextChange;

	LogoutButton.OnClick := @DoLogout;
	DoLoadCharacterList;
end;

procedure TViewCharacterList.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
	inherited;
end;

function TViewCharacterList.Press(const Event: TInputPressRelease): Boolean;
begin
	Result := inherited;
	if Result then Exit; // allow the ancestor to handle keys

	{ This virtual method is executed when user presses
		a key, a mouse button, or touches a touch-screen.

		Note that each UI control has also events like OnPress and OnClick.
		These events can be used to handle the "press", if it should do something
		specific when used in that UI control.
		The TViewCharacterList.Press method should be used to handle keys
		not handled in children controls.
	}

	// Use this to handle keys:
	{
	if Event.IsKey(keyXxx) then
	begin
		// DoSomething;
		Exit(true); // key was handled
	end;
	}
end;

procedure TViewCharacterList.DoLogout(Sender: TObject);
begin
	GlobalClient.Disconnect;
end;

procedure TViewCharacterList.DoLoadCharacterList();
begin
	// TODO: Show "loading" at the middle of the screen while character list is loaded

	GlobalClient.Send(TMsgCharacterList, DummyModel, @OnCharacterList);
end;

procedure TViewCharacterList.OnCharacterList(const ACharacterList: TModelBase);
var
	LCharacter: TMsgResCharacter;
	LSelection: TCharacterSelection;
	LInner: TCastleUserInterface;
begin
	for LCharacter in (ACharacterList as TMsgResCharacterList).list do begin
		LSelection := TCharacterSelection.Create(CharacterList);
		CharacterList.InsertFront(LSelection);
		LSelection.URL := 'castle-data:/componentcharacterbutton.castle-user-interface';

		LInner := LSelection.FindRequiredComponent('CharacterButton') as TCastleUserInterface;
		LSelection.Width := LInner.Width;
		LSelection.Height := LInner.Height;

		(LSelection.FindRequiredComponent('CharacterName') as TCastleLabel)
			.Caption := LCharacter.name;
		(LSelection.FindRequiredComponent('CharacterClass') as TCastleLabel)
			.Caption := LoreCollection.GetById(LCharacter.&class).LoreName;

		LSelection.Id := LCharacter.id;

		LSelection.OnRelease := @DoEnterGame;
	end;
end;

procedure TViewCharacterList.DoEnterGame(const Ui: TCastleUserInterface; const Event: TInputPressRelease; var Handled: Boolean);
var
	LModel: TMsgEnterGame;
begin
	Handled := false;

	if Event.isMouseButton(buttonLeft) then begin
		Handled := true;

		FPlayerId := (Ui as TCharacterSelection).Id;

		LModel := TMsgEnterGame.Create;
		LModel.Value := FPlayerId;

		GlobalClient.Send(TMsgEnterGame, LModel, @OnEnterGame);

		LModel.Free;
	end;
end;

procedure TViewCharacterList.OnEnterGame(const Success: TModelBase);
begin
	if (Success as TMsgResSuccess).Value = '1' then begin
		StartLoading(Container, FPlayerId);
	end
	else begin
		LogDebug('Failure while trying to enter the game');
		// TODO: notify user something's wrong
	end;
end;

{ implementation end }

end.


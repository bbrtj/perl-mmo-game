unit GameViewCharacterList;

interface

uses SysUtils, Classes,
	CastleVectors, CastleComponentSerialize,
	CastleUIControls, CastleControls, CastleKeysMouse,
	CastleFonts, CastleStringUtils, CastleUnicode,
	GameTypes,
	GameUIComponents,
	GameLore,
	GameNetwork,
	GameModels, GameModels.General, GameModels.Logout, GameModels.CharacterList, GameModels.EnterGame;

type

	TCharacterSelection = class(TCastleDesign)
	private
		FId: TUlid;

	public
		property Id: TUlid read FId write FId;
	end;


	TViewCharacterList = class(TCastleView)
	private
		FCharacterList: TCastleVerticalGroup;
		FLogoutButton: TGameButton;

	public
		constructor Create(vOwner: TComponent); override;
		procedure Start; override;
		procedure Update(const vSecondsPassed: Single; var vHandleInput: Boolean); override;
		function Press(const Event: TInputPressRelease): Boolean; override;

		procedure DoLogout(vSender: TObject);

		procedure DoLoadCharacterList();
		procedure OnCharacterList(const vCharacterList: TModelBase);

		procedure DoEnterGame(const vUi: TCastleUserInterface; const vEvent: TInputPressRelease; var vHandled: Boolean);
		procedure OnEnterGame(const vSuccess: TModelBase);

	end;

var
	ViewCharacterList: TViewCharacterList;

implementation

uses GameViewLogin,
	GameViewLoading;

{ TViewCharacterList ----------------------------------------------------------------- }

constructor TViewCharacterList.Create(vOwner: TComponent);
begin
	inherited;
	DesignUrl := 'castle-data:/gameviewcharacterlist.castle-user-interface';
end;

procedure TViewCharacterList.Start;
begin
	inherited;
	GlobalClient.ContextChange;

	FCharacterList := DesignedComponent('CharacterList') as TCastleVerticalGroup;
	FLogoutButton := DesignedComponent('LogoutButton') as TGameButton;

	FLogoutButton.OnClick := @DoLogout;
	DoLoadCharacterList;
end;

procedure TViewCharacterList.Update(const vSecondsPassed: Single; var vHandleInput: Boolean);
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

procedure TViewCharacterList.DoLogout(vSender: TObject);
begin
	GlobalClient.Send(TMsgLogout, DummyModel);

	Container.View := ViewLogin;
end;

procedure TViewCharacterList.DoLoadCharacterList();
begin
	// TODO: notify of loading

	GlobalClient.Send(TMsgCharacterList, DummyModel, @OnCharacterList);
end;

procedure TViewCharacterList.OnCharacterList(const vCharacterList: TModelBase);
var
	vCharacter: TMsgResCharacter;
	vSelection: TCharacterSelection;
	vInner: TCastleUserInterface;
begin
	for vCharacter in (vCharacterList as TMsgResCharacterList).list do begin
		vSelection := TCharacterSelection.Create(FCharacterList);
		FCharacterList.InsertFront(vSelection);
		vSelection.URL := 'castle-data:/componentcharacterbutton.castle-user-interface';

		vInner := vSelection.FindRequiredComponent('CharacterButton') as TCastleUserInterface;
		vSelection.Width := vInner.Width;
		vSelection.Height := vInner.Height;

		(vSelection.FindRequiredComponent('CharacterName') as TCastleLabel)
			.Caption := vCharacter.name;
		(vSelection.FindRequiredComponent('CharacterClass') as TCastleLabel)
			.Caption := LoreCollection.GetById(vCharacter.&class).LoreName;

		vSelection.Id := vCharacter.id;

		vSelection.OnRelease := @DoEnterGame;
		// TODO: vDesign needs freeing?
	end;
end;

procedure TViewCharacterList.DoEnterGame(const vUi: TCastleUserInterface; const vEvent: TInputPressRelease; var vHandled: Boolean);
var
	vModel: TPlaintextModel;
begin
	vHandled := false;

	if vEvent.isMouseButton(buttonLeft) then begin
		vHandled := true;
		vModel := TPlaintextModel.Create;
		vModel.Value := (vUi as TCharacterSelection).Id;

		GlobalClient.Send(TMsgEnterGame, vModel, @OnEnterGame);

		vModel.Free;
	end;
end;

procedure TViewCharacterList.OnEnterGame(const vSuccess: TModelBase);
begin
	if (vSuccess as TMsgResSuccess).Value = '1' then begin
		StartLoading(Container);
	end
	else begin
		writeln('failure!');
		// TODO: notify user something's wrong
	end;
end;

{ implementation end }

end.

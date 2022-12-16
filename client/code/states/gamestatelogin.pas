unit GameStateLogin;

interface

uses SysUtils, Classes,
	CastleVectors, CastleUIState, CastleComponentSerialize,
	CastleUIControls, CastleControls, CastleKeysMouse,
	CastleFonts, CastleStringUtils, CastleUnicode,
	GameUIComponents,
	GameTranslations,
	GameNetwork,
	GameModels, GameModels.General, GameModels.Login;

type
	TStateLogin = class(TUIState)
	private
		FUsernameField: TCastleEdit;
		FPasswordField: TCastleEdit;
		FLoginButton: TGameButton;
		FStatus: TCastleLabel;

	public
		constructor Create(AOwner: TComponent); override;
		procedure Start; override;
		procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
		function Press(const Event: TInputPressRelease): Boolean; override;

		procedure DoLogin(vSender: TObject);

		procedure OnConnected();
		procedure OnLogin(const vSuccess: TModelBase);
	end;

var
	StateLogin: TStateLogin;

implementation

uses GameStateCharacterList;

{ TStateLogin ----------------------------------------------------------------- }

constructor TStateLogin.Create(AOwner: TComponent);
begin
	inherited;
	DesignUrl := 'castle-data:/gamestatelogin.castle-user-interface';
end;

procedure TStateLogin.Start;
begin
	inherited;
	GlobalClient.ContextChange;

	{ Find components, by name, that we need to access from code }
	FUsernameField := DesignedComponent('UsernameField') as TCastleEdit;
	FPasswordField := DesignedComponent('PasswordField') as TCastleEdit;
	FLoginButton := DesignedComponent('LoginButton') as TGameButton;
	FStatus := DesignedComponent('LoginStatus') as TCastleLabel;

	FLoginButton.onClick := @DoLogin;
end;

procedure TStateLogin.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
	inherited;
end;

function TStateLogin.Press(const Event: TInputPressRelease): Boolean;
begin
	Result := inherited;
	if Result then Exit; // allow the ancestor to handle keys

	{ This virtual method is executed when user presses
		a key, a mouse button, or touches a touch-screen.

		Note that each UI control has also events like OnPress and OnClick.
		These events can be used to handle the "press", if it should do something
		specific when used in that UI control.
		The TStateLogin.Press method should be used to handle keys
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

procedure TStateLogin.DoLogin(vSender: TObject);
begin
	FStatus.Caption := _('msg.connecting');
	FStatus.Exists := true;

	GlobalClient.Connect(
		GlobalClient.cDefaultHost,
		GlobalClient.cDefaultPort,
		@onConnected
	);
end;

procedure TStateLogin.OnConnected();
var
	vData: TMsgLogin;
begin
	FStatus.Caption := _('msg.logging_in');

	vData := TMsgLogin.Create;

	vData.email := FUsernameField.Text;
	vData.password := FPasswordField.Text;

	GlobalClient.Send(TMsgLogin, vData, @onLogin);

	vData.Free;
end;

procedure TStateLogin.OnLogin(const vSuccess: TModelBase);
begin
	if (vSuccess as TMsgResSuccess).Value = '1' then begin
		TUIState.Current := StateCharacterList;
	end
	else begin
		FStatus.Caption := _('msg.login_failed');
		GlobalClient.Disconnect;
	end;
end;

end.


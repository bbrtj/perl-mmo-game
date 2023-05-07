unit GameViewLogin;

interface

uses SysUtils, Classes,
	CastleVectors, CastleComponentSerialize,
	CastleUIControls, CastleControls, CastleKeysMouse,
	CastleFonts, CastleStringUtils, CastleUnicode,
	GameUIComponents,
	GameTranslations,
	GameNetwork,
	GameModels, GameModels.General, GameModels.Login;

type
	TViewLogin = class(TCastleView)
	published
		UsernameField: TCastleEdit;
		PasswordField: TCastleEdit;
		LoginButton: TGameButton;
		LoginStatus: TCastleLabel;

	public
		constructor Create(vOwner: TComponent); override;
		procedure Start; override;
		procedure Update(const vSecondsPassed: Single; var vHandleInput: Boolean); override;
		function Press(const vEvent: TInputPressRelease): Boolean; override;

		procedure DoLogin(vSender: TObject);

		procedure OnConnected();
		procedure OnLogin(const vSuccess: TModelBase);
		procedure OnDisconnected();
	end;

var
	ViewLogin: TViewLogin;

implementation

uses GameViewCharacterList;

constructor TViewLogin.Create(vOwner: TComponent);
begin
	inherited;
	DesignUrl := 'castle-data:/gameviewlogin.castle-user-interface';
end;

procedure TViewLogin.Start;
begin
	inherited;
	GlobalClient.ContextChange;
	GlobalClient.OnDisconnected := @OnDisconnected;

	LoginButton.onClick := @DoLogin;
end;

procedure TViewLogin.Update(const vSecondsPassed: Single; var vHandleInput: Boolean);
begin
	inherited;
end;

function TViewLogin.Press(const vEvent: TInputPressRelease): Boolean;
begin
	Result := inherited;
	if Result then Exit; // allow the ancestor to handle keys

	{ This virtual method is executed when user presses
		a key, a mouse button, or touches a touch-screen.

		Note that each UI control has also events like OnPress and OnClick.
		These events can be used to handle the "press", if it should do something
		specific when used in that UI control.
		The TViewLogin.Press method should be used to handle keys
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

procedure TViewLogin.DoLogin(vSender: TObject);
begin
	LoginStatus.Caption := _('msg.connecting');
	LoginStatus.Exists := true;

	GlobalClient.Connect(@onConnected);
end;

procedure TViewLogin.OnConnected();
var
	vData: TMsgLogin;
begin
	LoginStatus.Caption := _('msg.logging_in');

	vData := TMsgLogin.Create;

	vData.email := UsernameField.Text;
	vData.password := PasswordField.Text;

	GlobalClient.Send(TMsgLogin, vData, @onLogin);

	vData.Free;
end;

procedure TViewLogin.OnLogin(const vSuccess: TModelBase);
begin
	if (vSuccess as TMsgResSuccess).Value = '1' then begin
		Container.View := ViewCharacterList;
	end
	else begin
		LoginStatus.Caption := _('msg.login_failed');
		GlobalClient.Disconnect(False);
	end;
end;

procedure TViewLogin.OnDisconnected();
begin
	Container.View := self;
	LoginStatus.Caption := _('msg.disconnected');
	LoginStatus.Exists := True;
end;

{ implementation end }

end.


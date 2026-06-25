unit GamePipelines;

interface

uses SysUtils, Classes, Generics.Collections,
	GameTypes, GameLog;

type
	EPipeline = class(Exception);

	TPipelineStatus = (
		psNew,
		psStarted,
		psFailed,
		psFinished
	);

	TPipeline = class abstract
	private
		FStatus: TPipelineStatus;
		FOnFinish: TNotifyEvent;
		FOnFail: TNotifyEvent;
	protected
		procedure Finish(Sender: TObject); virtual;
		procedure Fail(Sender: TObject); virtual;
	public
		procedure Start(Sender: TObject); virtual;
		procedure SetNext(Target: TNotifyEvent);
		procedure SetNext(Target: TPipeline);
		procedure SetFail(Target: TNotifyEvent);
		procedure SetFail(Target: TPipeline);
	public
		property Status: TPipelineStatus read FStatus write FStatus;
	end;

	TPipelineClass = class of TPipeline;

	TKnownPipeline = record
		Pipeline: TPipeline;
		LastStatus: TPipelineStatus;
		VisitedCount: Integer;
	end;

	TPipelineList = specialize TList<TKnownPipeline>;
	TPipelineManager = class
	private
		FPipelines: TPipelineList;
	public
		constructor Create();
		destructor Destroy; override;
	public
		function New(Typ: TPipelineClass): TPipeline;
		procedure Cleanup();
	end;

	TCreatePipelineProc = function(Arg: TObject): TPipeline of object;
	TForkPipeline = class(TPipeline)
	private
		FBuildPipelineProc: TCreatePipelineProc;
		FPipelinesCount: Integer;
	protected
		function BuildPipeline(Arg: TObject): TPipeline; virtual;
		procedure Finish(Sender: TObject); override;
	public
		procedure Start(Sender: TObject); override;
	public
		property BuildPipelineProc: TCreatePipelineProc read FBuildPipelineProc write FBuildPipelineProc;
	end;

	TSenderWithArg = class
	public
		Sender: TObject;
		Arg: Byte;
		constructor Create(ASender: TObject; AArg: Byte);
	end;

	TConditionPipeline = class(TPipeline)
	private
		FOnArg: Array of TNotifyEvent;
	public
		procedure Start(Sender: TObject); override;
		procedure SetNext(Target: TNotifyEvent; Arg: Byte);
		procedure SetNext(Target: TPipeline; Arg: Byte);
	end;

var
	GlobalPipelineManager: TPipelineManager;

implementation

procedure TPipeline.Finish(Sender: TObject);
begin
	if FOnFinish <> nil then
		FOnFinish(Sender);
	FStatus := psFinished;
end;

procedure TPipeline.Fail(Sender: TObject);
begin
	if FOnFail <> nil then
		FOnFail(Sender);
	FStatus := psFailed;
end;

procedure TPipeline.Start(Sender: TObject);
begin
	FStatus := psStarted;
end;

procedure TPipeline.SetNext(Target: TNotifyEvent);
begin
	FOnFinish := Target;
end;

procedure TPipeline.SetNext(Target: TPipeline);
begin
	FOnFinish := @Target.Start;
end;

procedure TPipeline.SetFail(Target: TNotifyEvent);
begin
	FOnFail := Target;
end;

procedure TPipeline.SetFail(Target: TPipeline);
begin
	FOnFail := @Target.Start;
end;

constructor TPipelineManager.Create();
begin
	FPipelines := TPipelineList.Create;
end;

destructor TPipelineManager.Destroy();
var
	I: Integer;
begin
	if FPipelines <> nil then begin
		for I := 0 to FPipelines.Count - 1 do
			FPipelines[I].Pipeline.Free;

		FPipelines.Free;
	end;

	inherited;
end;

function TPipelineManager.New(Typ: TPipelineClass): TPipeline;
var
	LRec: TKnownPipeline;
begin
	result := Typ.Create;

	LRec.Pipeline := result;
	LRec.VisitedCount := 0;
	LRec.LastStatus := result.Status;
	FPipelines.Add(LRec);
end;

{ Performs a periodic cleanup }
procedure TPipelineManager.Cleanup();
const
	CLostThreshold = 1;
	CStuckThreshold = 5;
var
	I: Integer;
	LRemove: Boolean;
	LRec: TKnownPipeline;
begin
	for I := FPipelines.Count - 1 downto 0 do begin
		LRec := FPipelines[I];
		LRemove :=
			(LRec.Pipeline.Status = psFailed)
			or (LRec.Pipeline.Status = psFinished)
			or (
				(LRec.Pipeline.Status = psNew)
				and (LRec.VisitedCount >= CLostThreshold)
			)
			or (
				(LRec.Pipeline.Status = psStarted)
				and (LRec.LastStatus = psStarted)
				and (LRec.VisitedCount >= CStuckThreshold)
			);

		if LRemove then begin
			LogDebug(
				format(
					'Removing a pipeline of type %s, status %d, seen %d times',
					[
						LRec.Pipeline.ClassName,
						Ord(LRec.Pipeline.Status),
						LRec.VisitedCount
					]
				)
			);
			LRec.Pipeline.Free;
			FPipelines.Delete(I);
		end
		else begin
			Inc(LRec.VisitedCount);
			LRec.LastStatus := LRec.Pipeline.Status;
			FPipelines[I] := LRec;
		end;
	end;
end;

function TForkPipeline.BuildPipeline(Arg: TObject): TPipeline;
begin
	result := FBuildPipelineProc(Arg);
	if result <> nil then
		Inc(FPipelinesCount);
end;

procedure TForkPipeline.Finish(Sender: TObject);
begin
	Dec(FPipelinesCount);

	if FPipelinesCount <= 0 then
		inherited Finish(self);
end;

procedure TForkPipeline.Start(Sender: TObject);
var
	LPipeline: TPipeline;
begin
	inherited;
	FPipelinesCount := 0;

	LPipeline := self.BuildPipeline(Sender);
	while LPipeline <> nil do begin
		LPipeline.SetNext(@self.Finish);
		LPipeline.Start(nil);
		LPipeline := self.BuildPipeline(Sender);
	end;

	// if there is nothing to fork, make sure to just push the pipeline further
	if FPipelinesCount = 0 then
		self.Finish(nil);
end;

constructor TSenderWithArg.Create(ASender: TObject; AArg: Byte);
begin
	self.Sender := ASender;
	self.Arg := AArg;
end;

procedure TConditionPipeline.Start(Sender: TObject);
var
	Arg: Byte;
	RealSender: TObject;
begin
	inherited;

	try
		if not(Sender is TSenderWithArg) then
			raise EPipeline.Create('Condition pipeline called without a proper argument');

		Arg := TSenderWithArg(Sender).Arg;
		RealSender := TSenderWithArg(Sender).Sender;
		Sender.Free;

		if Arg > High(FOnArg) then
			raise EPipeline.Create('Condition pipeline has no handler for argument ' + IntToStr(Arg));

		FOnArg[Arg](RealSender);
		self.Finish(RealSender);
	except
		on E: EPipeline do self.Fail(E);
	end;
end;

procedure TConditionPipeline.SetNext(Target: TNotifyEvent; Arg: Byte);
begin
	if Arg > High(FOnArg) then
		SetLength(FOnArg, Arg + 1);
	FOnArg[Arg] := Target;
end;

procedure TConditionPipeline.SetNext(Target: TPipeline; Arg: Byte);
begin
	if Arg > High(FOnArg) then
		SetLength(FOnArg, Arg + 1);
	FOnArg[Arg] := @Target.Start;
end;

initialization
	GlobalPipelineManager := TPipelineManager.Create;

finalization
	FreeAndNil(GlobalPipelineManager);

end.


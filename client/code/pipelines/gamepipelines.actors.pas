unit GamePipelines.Actors;

interface

uses SysUtils, Classes, Generics.Collections,
	GameTypes, GamePipelines;

type
	TRequestActorInfoPipeline = class(TPipeline)
	private
		FActorId: TUlid;
	private
		procedure RecordAvailable(Sender: TObject);
	public
		procedure Start(Sender: TObject); override;
	public
		property ActorId: TUlid read FActorId write FActorId;
	end;

implementation

uses GameActors;

procedure TRequestActorInfoPipeline.Start(Sender: TObject);
begin
	GlobalActorRepository.RequestActorInfo(FActorId, @self.RecordAvailable);
end;

procedure TRequestActorInfoPipeline.RecordAvailable(Sender: TObject);
var
	LActorRecord: TGameActorRepositoryRecord;
begin
	LActorRecord := GlobalActorRepository.GetActorInfo(FActorId);
	self.Finish(LActorRecord);
end;

end.


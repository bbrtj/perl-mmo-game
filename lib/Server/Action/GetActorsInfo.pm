package Server::Action::GetActorsInfo;

use My::Moose;
use Resource::ActorsInfo;
use all 'Model';

use header;

extends 'Server::GameAction';

use constant name => 'get_actors_info';
use constant required_state => Model::PlayerSession->STATE_PLAYING;

sub validate ($self, $data)
{
	state $type = Types::ArrayRef [Types::ULID];
	my $parts = [split quotemeta Server::Config::PROTOCOL_SEPARATOR, $data];

	$type->assert_valid($parts);

	return $parts;
}

sub handle ($self, $player_id, $id, $wanted_actors)
{
	my $actors_aref = $self->game_process->server->actors_info($player_id, $wanted_actors);

	$self->send_to_player(
		$player_id,
		Resource::ActorsInfo->new(
			subject => $actors_aref,
		),
		id => $id,
	);

	return;
}


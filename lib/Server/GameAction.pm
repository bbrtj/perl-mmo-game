package Server::GameAction;

use My::Moose;
use all 'Model', 'Resource';

use header;

extends 'Server::Action';

with qw(
	Server::Role::WithGameProcess
);

use constant required_state => Model::PlayerSession->STATE_PLAYING;

before handle => sub ($self, $player_id, $id, @params) {
	try {
		$self->can('checks') && $self->checks($player_id, @params);
	}
	catch ($e) {
		$self->send_to_player(
			$player_id,
			Resource::X->new($e),
			id => $id,
		) if $e isa 'X::CheckFailed';

		die $e;
	}
};

sub send_to_player ($self, @params)
{
	return $self->game_process->send_to_player(@params);
}


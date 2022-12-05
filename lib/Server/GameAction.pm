package Server::GameAction;

use My::Moose;
use all 'Model';
use Resource::FailedCheck;

use header;

extends 'Server::Action';

with qw(
	Server::Role::WithGameProcess
);

use constant required_state => Model::PlayerSession->STATE_PLAYING;

around handle => sub ($orig, $self, $player_id, $id, @params) {
	if ($self->can('checks')) {
		my $check = $self->checks($player_id, @params);

		if (!$check->result) {
			return $self->send_to_player(
				$player_id,
				Resource::FailedCheck->new($check),
				id => $id,
			);

			return;
		}
	}

	return $self->$orig($player_id, $id, @params);
};

sub send_to_player ($self, @params)
{
	return $self->game_process->send_to_player(@params);
}


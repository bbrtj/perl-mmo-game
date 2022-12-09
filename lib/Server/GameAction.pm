package Server::GameAction;

use My::Moose;
use all 'Model', 'Resource';

use header;

extends 'Server::Action';

with qw(
	Server::Role::WithGameProcess
);

use constant required_state => Model::PlayerSession->STATE_PLAYING;

sub send_to_player ($self, @params)
{
	return $self->game_process->send_to_player(@params);
}


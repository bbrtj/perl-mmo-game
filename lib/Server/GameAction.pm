package Server::GameAction;

use My::Moose;
use all 'Model';

use header;

extends 'Server::Action';

with qw(
	Server::Role::WithGameProcess
);

use constant required_state => Model::PlayerSession->STATE_PLAYING;


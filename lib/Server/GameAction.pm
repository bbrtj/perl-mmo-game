package Server::GameAction;

use My::Moose;
use Model::PlayerSession;

use header;

extends 'Server::Action';

use constant required_state => Model::PlayerSession->STATE_PLAYING;


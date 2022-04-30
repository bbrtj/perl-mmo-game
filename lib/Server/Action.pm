package Server::Action;

use My::Moose;
use Model::PlayerSession;

use header;

extends 'Server::Command';

use constant required_state => Model::PlayerSession->STATE_PLAYING;


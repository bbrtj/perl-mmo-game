package Server::Role::WithGameProcess;

use My::Moose::Role;

use header;

has field 'game_process' => (
	isa => Types::InstanceOf ['Server::Process::Game'],
	writer => 1,
	weak_ref => 1,
);


package Service::DataBus;

use My::Moose;

use header;

extends 'Service::Channel';

sub broadcast ($self, $name, @args)
{
	$self->SUPER::broadcast(undef, [Types::ULID::ulid, $name, @args]);

	return;
}

sub dispatch ($self, $location, $name, @args)
{
	$self->SUPER::broadcast($location, [$name, @args]);

	return;
}

sub emit ($self, $processable, $session, @args)
{
	if ($processable->does('Server::Role::WithGameProcess')) {
		$self->dispatch($session->location_id, $processable->name, ($session->player_id, @args));
	}
	else {
		$self->broadcast($processable->name, ($session->id, @args));
	}

	return;
}

=pod

This is a communication channel that is specifically used by the worker
(Server::Worker) to communicate with child processes.


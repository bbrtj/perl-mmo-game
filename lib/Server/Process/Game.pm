package Server::Process::Game;

use My::Moose;
use Data::Dumper;
use Server::Config;

use header;

extends 'Server::Process';

has param 'location_data' => (
	coerce => (Types::InstanceOf ['Unit::Location'])
		->plus_coercions(
			Types::InstanceOf ['Game::Lore::Location'], q{ DI->get('units_repo')->load_location($_->id) }
		),
);

with qw(
	Server::Role::Listening
	Server::Role::CanSendData
);

sub handle ($self, $data)
{
	my ($name, @args) = $data->@*;

	$self->worker->log->debug("Got an action / event: $name")
		if Server::Config::DEBUG;

	my $instance =
		$self->worker->get_action($name)
		// $self->worker->get_event($name)
		;

	if (!defined $instance || !$instance->does('Server::Role::WithGameProcess')) {
		$self->worker->log->error("Unknown game handler name $name");
		return;
	}

	$self->worker->log->debug('Game process for location ' . $self->location_data->location->id . ": processing $name");
	try {
		$instance->set_game_process($self);
		$instance->handle(@args);
	}
	catch ($e) {
		$self->worker->log->error("Processing game handler $name failed: $e");
		$self->worker->log->debug("Error was: " . Dumper($e));
	}

	return;
}

sub do_work ($self)
{
	$self->_listen(
		$self->worker->data_bus,
		$self->location_data->location->id,
		sub ($data) {
			$self->handle($data);
		}
	);

	$self->worker->log->info('Game process for ' . $self->location_data->location->id . ' started');
	return;
}

# TODO: run periodically
sub save_work ($self)
{
	DI->get('units_repo')->save($self->location_data);
	$self->worker->log->info('Game data for ' . $self->location_data->location->id . ' saved');

	return;
}

sub finish_work ($self)
{
	# Save location data here! Otherwise we leak it
	$self->save_work;

	$self->_unlisten;

	return;
}


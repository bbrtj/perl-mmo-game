package Server::Worker::Process::Game;

use My::Moose;
use Data::Dumper;

use header;

extends 'Server::Worker::Process';

has param 'location_data' => (
	coerce => (Types::InstanceOf ['Unit::Location'])
		->plus_coercions(
			Types::InstanceOf ['Game::Lore::Location'], q{ DI->get('units')->load_location($_->id) }
		),
);

# TODO: remote signals (to synchronize locations: who enters it? who exits?)

sub handle ($self, $data)
{
	my ($name, @args) = $data->@*;

	my $instance = $self->worker->actions->{$name};

	if (!defined $instance) {
		$self->worker->log->error("Unknown game handler name $name");
		return;
	}

	$self->worker->log->debug('Game process for location ' . $self->location_data->location->id . ": processing $name");
	try {
		# pass location data to the handler as the last argument
		$instance->handle(@args, $self->location_data);
	}
	catch ($e) {
		$self->worker->log->error("Processing game handler $name failed: $e");
		$self->worker->log->debug("Error was: " . Dumper($e));
	}

	return;
}

sub do_work ($self)
{
	$self->{_cb} = $self->worker->channel->listen(
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
	DI->get('units')->save($self->location_data);
	$self->worker->log->info('Game data for ' . $self->location_data->location->id . ' saved');

	return;
}

sub finish_work ($self)
{
	# Save location data here! Otherwise we leak it
	$self->save_work;
	$self->worker->channel->unlisten($self->location_data->location->id, $self->{_cb});

	return;
}


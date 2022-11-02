package Server::Worker::Process::Game;

use My::Moose;
use Data::Dumper;
use Sub::HandlesVia;

use header;

extends 'Server::Worker::Process';

has injected 'data_bus' => as => 'data_channel_service';

has param 'location_data' => (
	coerce => (Types::InstanceOf ['Unit::Location'])
		->plus_coercions(
			Types::InstanceOf ['Game::Lore::Location'], q{ DI->get('units')->load_location($_->id) }
		),
);

has field '_callbacks' => (
	isa => Types::ArrayRef [Types::CodeRef],
	default => sub { [] },
	'handles[]' => {
		'_add_callback' => sub ($callbacks, $channel, $id, $handler) {
			my $wrapped = $channel->listen($id, $handler);
			push $callbacks->@*, sub { $channel->unlisten($id, $wrapped) };
		},
		'_all_callbacks' => 'all',
	},
);

sub handle_action ($self, $data)
{
	my ($name, @args) = $data->@*;

	my $instance = $self->worker->get_action($name);

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

sub handle_data ($self, $data)
{
	# TODO: run event
}

sub do_work ($self)
{
	$self->_add_callback(
		$self->worker->channel,
		$self->location_data->location->id,
		sub ($data) {
			$self->handle_action($data);
		}
	);

	$self->_add_callback(
		$self->data_bus,
		$self->location_data->location->id,
		sub ($data) {
			$self->handle_data($data);
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

	$_->() foreach $self->_all_callbacks;

	return;
}


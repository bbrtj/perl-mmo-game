package Server::Process::Game;

use My::Moose;
use Data::Dumper;
use Server::Config;
use Game::Server;
use Time::HiRes qw(time);

use header;

extends 'Server::Process';

has injected 'sessions_cache' => (
	'handles->' => {
		'save_session' => 'save',
		'load_session' => 'load',
		'remove_session' => 'remove',
	}
);

has param 'location_id' => (
	isa => Types::LoreId,
);

has field 'server' => (
	isa => Types::InstanceOf ['Game::Server'],
	default => sub ($self) {
		return Game::Server->new(
			process => $self,
			location_data => DI->get('units_repo')->load_location($self->location_id)
		);
	},
);

with qw(
	Server::Role::Listening
	Server::Role::CanSendData
);

sub send_to_actor ($self, $actor, $data, @more)
{
	my $session_id = $self->load_session($actor->player->id);

	return $self->send_to($session_id, $data, @more);
}

sub handle ($self, $data)
{
	my ($name, @args) = $data->@*;

	$self->log->debug("Got an action / event: $name")
		if Server::Config::DEBUG;

	my $instance =
		$self->worker->get_action($name)
		// $self->worker->get_event($name)
		;

	if (!defined $instance || !$instance->does('Server::Role::WithGameProcess')) {
		$self->log->error("Unknown game handler name $name");
		return;
	}

	$self->log->debug('Game process for location ' . $self->location_id . ": processing $name");
	try {
		$instance->set_game_process($self);
		$instance->handle(@args);
	}
	catch ($e) {
		$self->log->error("Processing game handler $name failed: $e");
		$self->log->debug("Error was: " . Dumper($e));
	}

	return;
}

sub do_work ($self, $loop)
{
	$self->_listen(
		$self->worker->data_bus,
		$self->location_id,
		sub ($data) {
			$self->handle($data);
		}
	);

	my $tick = Server::Config::SERVER_TICK;
	my $min_tick = $tick / 10;
	my $elapsed = 0;
	my $start;

	my $tick_sref;
	my sub next_tick_setup ()
	{
		my $after = ($elapsed + 1) * $tick - (time() - $start);

		if (Server::Config::DEBUG) {
			my $processing_time = abs($after - $tick);
			my $alert = $processing_time > $tick / 2 ? ' [!!]' : '';
			$self->log->debug($self->location_id . ": last processing took $processing_time$alert");
		}

		# NOTE: $min_tick instead of 0 not to hijack entire event loop
		$after = $min_tick if $after < $min_tick;
		$loop->timer($after => $tick_sref);

		return;
	}

	$tick_sref = sub {
		try {
			$self->server->tick(++$elapsed);
		}
		catch ($e) {
			$self->log->error("Error occured in server processing loop: $e");
		}

		next_tick_setup();
	};

	$loop->next_tick(
		sub {
			$start = $self->server->start_time;
			next_tick_setup();
		}
	);

	$self->log->info('Game process for ' . $self->location_id . ' started');
	return;
}

# TODO: run periodically
sub save_work ($self)
{
	DI->get('units_repo')->save($self->server->location_data);
	$self->log->info('Game data for ' . $self->location_id . ' saved');

	return;
}

sub finish_work ($self)
{
	# Save location data here! Otherwise we leak it
	$self->save_work;

	$self->_unlisten;

	return;
}


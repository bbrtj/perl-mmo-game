package Server::Process::Game;

use My::Moose;
use Server::Config;
use Game::Server;
use List::Util qw(max);

use all 'X';

use header;

extends 'Server::Process';

has param 'location_id' => (
	isa => Types::LoreId,
);

has field 'server' => (
	isa => Types::InstanceOf ['Game::Server'],
	default => sub ($self) {
		return Game::Server->new(
			process => $self,
			location => DI->get('units_repo')->load_location($self->location_id)
		);
	},
);

has cached '_sessions' => (
	default => sub { {} },
	'handles{}' => {
		'save_session' => 'set',
		'load_session' => 'get',
		'remove_session' => 'delete',
	},
);

with qw(
	Server::Role::Listening
	Server::Role::CanSendData
);

sub send_to_player ($self, $player_id, $data, @more)
{
	return $self->send_to($self->load_session($player_id), $data, @more);
}

sub send_to_players ($self, $player_ids, $data)
{
	my @ids = $player_ids->@*;
	return if !@ids;

	return $self->send_to_player($ids[0], $data)
		if @ids == 1;

	return $self->send_to_all($data, sessions => [map { $self->load_session($_) } @ids]);
}

sub handle ($self, $data)
{
	my ($name, $player_id, $id, @args) = $data->@*;

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

	$self->log->debug('Game process for location ' . $self->location_id . ": processing $name (for $player_id)")
		if Server::Config::DEBUG;

	try {
		$instance->set_game_process($self);
		$instance->handle($player_id, $id, @args);
	}
	catch ($e) {
		if ($e isa 'X::Pub') {
			$self->send_to_player(
				$player_id,
				Resource::X->new(subject => $e),
				id => $id,
			);
		}
		else {
			$self->log->error("Processing game handler $name failed: $e");
			$self->log->debug("Error was: " . My::Dumper->dd($e))
				if Server::Config::DEBUG;
		}
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
	my $elapsed = 0;

	my $tick_sref;
	my sub next_tick_setup ()
	{
		$loop->timer($tick => $tick_sref);
		return;
	}

	$tick_sref = sub {
		my $start = server_time;

		try {
			$self->server->tick(++$elapsed);
		}
		catch ($e) {
			$self->log->error("Error occured in server processing loop: $e");
		}

		if (Server::Config::DEBUG) {
			my $processing_time = server_time - $start;

			my $alert = '';
			for (0.5, 1, 1.5) {
				$alert .= '!' if $processing_time > $tick / $_;
			}

			if ($alert) {
				$self->log->debug($self->location_id . ": last processing took $processing_time [$alert]");
			}
		}

		next_tick_setup();
	};

	$loop->next_tick(
		sub {
			next_tick_setup();
		}
	);

	$self->log->info('Game process for ' . $self->location_id . ' started');
	return;
}

# TODO: run periodically
sub save_work ($self)
{
	DI->get('units_repo')->save($self->server->location);
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


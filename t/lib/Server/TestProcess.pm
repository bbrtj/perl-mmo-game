package Server::TestProcess;

use My::Moose;

use Time::HiRes;
use Log::Handler;

use Game::Lore::Location;
use Unit::Location;
use Game::Server;
use ServerTime qw(mock_server_time);
use Server::Config;
use Component::Log;

use header;

has field 'sent_data' => (
	default => sub { [] },
);

has field 'log' => (
	builder => 1,
);

has field 'server' => (
	builder => 1,
	handles => [
		qw(
			location
		)
	],
);

has field 'start_time' => (
	default => sub { time },
);

has field 'last_tick' => (
	writer => 1,
	default => 0,
);

sub _build_log ($self)
{
	return Log::Handler->new(
		screen => {
			log_to => 'STDOUT',
			'utf-8' => true,
			maxlevel => 'debug',
			minlevel => 'critical',
			message_layout => '%m',
			message_pattern => [qw(%L %m)],
			prepare_message => Component::Log->_get_screen_callback('test'),
		}
	);
}

sub _build_server ($self)
{
	my $location_lore = Game::Lore::Location->new(
		id => 'loc.testmap',
		name => 'Test map',
		map => 'test_map',
	);

	return Game::Server->new(
		process => $self,
		location => Unit::Location->new(
			lore => $location_lore,
		),
	);
}

sub send_to_player ($self, $player_id, $data, @more)
{
	push $self->sent_data->@*, [$player_id, $data, @more];
}

sub send_to_players ($self, $player_ids, $data)
{
	foreach my $player_id ($player_ids->@*) {
		$self->send_to_player($player_id, $data);
	}
}

sub ticks ($self, $count = 1)
{
	my $tick = $self->last_tick;
	my $start_time = $self->start_time;

	for (1 .. $count) {
		mock_server_time($start_time + $tick * Server::Config::TICK);
		$self->server->tick(++$tick);
	}

	$self->set_last_tick($tick);
}


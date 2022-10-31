package Server::Worker;

use My::Moose;
use Server::Worker::Process::Game;
use Server::Worker::Process::Jobs;
use Data::ULID qw(ulid);
use Mojo::IOLoop;
use Mojo::Loader qw(load_classes);
use POSIX qw(ceil);
use List::Util qw(shuffle);

use header;

with qw(
	Server::Forked
);

has DI->injected('channel' => 'worker_channel_service');

# helper for attributes
my sub load_enabled ($namespace)
{
	return {
		map {
			$_->name => $_->new
		} grep { !$_->disabled } load_classes($namespace)
	};
}


# Jobs are internal only and ran in intervals
# (basically an advanced cron)
has field 'jobs' => (
	isa => Types::HashRef [Types::InstanceOf ['Server::Job']],
	default => sub { load_enabled('Server::Job') },
);

# Commands are events that need handling
has field 'commands' => (
	isa => Types::HashRef [Types::InstanceOf ['Server::Command']],
	default => sub { load_enabled('Server::Command') },
);

# Actions are what players call to play the game
has field 'actions' => (
	isa => Types::HashRef [Types::InstanceOf ['Server::Action']],
	default => sub { load_enabled('Server::Action') },
);

# keeps the worker alive in case of no specified commands
# can perform synchronization jobs
sub cleanup ($self)
{
	$self->log->info('Cleaning up...');
	Mojo::IOLoop->timer(60, sub { $self->cleanup });

	return;
}

sub _location_dispenser ($self, $game_processes)
{
	my $locations_href = DI->get('lore_data')->load_all_named('Game::Lore::Location');
	my @locations = shuffle values $locations_href->%*;
	my $processes_left = $game_processes;

	return sub {
		return () if !$processes_left;
		return () if @locations == 0;

		my $to_get = ceil(@locations / $processes_left);
		$processes_left -= 1;

		return splice @locations, 0, $to_get;
	};
}

sub start ($self, $processes = 2)
{
	die 'must have at least 2 processes'
		unless $processes > 1;

	# keep at least 75% of processes as game processes
	my $jobs_processes = (int $processes / 4) || 1;
	my $game_processes = $processes - $jobs_processes;

	# create processes for shared jobs
	$self->create_forks(
		'job',
		$jobs_processes,
		sub ($process_id) {
			Server::Worker::Process::Jobs->new(
				worker => $self,
				process_id => $process_id
			)->do_work;
		}
	);

	# create processes for game servers
	my $get_locations = $self->_location_dispenser($game_processes);
	$self->create_forks(
		'game',
		$game_processes,
		sub ($process_id) {
			my @processes = map {
				Server::Worker::Process::Game->new(
					worker => $self,
					process_id => $process_id,
					location_data => $_,
				)
			} $get_locations->();

			$_->do_work for @processes;

			Mojo::IOLoop->start;

			$_->finish_work for @processes;
		},
		$get_locations
	);

	# setup crons
	my $setup_job;
	$setup_job = sub ($job) {
		Mojo::IOLoop->timer(
			$job->interval,
			sub {
				$self->broadcast($job->name);
				$setup_job->($job);
			}
		);
	};

	foreach my $job (values $self->jobs->%*) {
		$setup_job->($job)
			if $job->interval;
	}

	$self->cleanup;
	return;
}

sub broadcast ($self, $name, @args)
{
	$self->channel->broadcast(undef, [ulid, $name, @args]);

	return;
}

sub broadcast_action ($self, $location, $name, @args)
{
	$self->channel->broadcast($location, [$name, @args]);

	return;
}

__END__

=pod

Worker is basically an internal server synchronizer and cron job scheduler. It
spawns processes, and then broadcasts jobs, commands and actions to them. The
first child process to store a redis key with job / command ULID gets to
process it. In case of actions, only one action at once will be listening to
certain location events.

Some processes work on cron-like and cleaup tasks (job processers), while
others take care of actual game logic. Each game location is held within one
process to take care of its events and keep its internal state, which is then
synchronized to the database in intervals.


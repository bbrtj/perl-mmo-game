package Server::Worker;

use My::Moose;
use Server::Worker::Process::Game;
use Server::Worker::Process::Jobs;
use Data::ULID qw(ulid);
use Mojo::IOLoop;
use POSIX qw(ceil);
use List::Util qw(shuffle);
use Sub::HandlesVia;

use header;

with qw(
	Server::Forked
);

has injected 'channel' => as => 'worker_channel_service';

# All registered processables
has field 'processable' => (
	constructed => ['Server::ProcessableList'],
	'handles->' => {
		'get_processable' => 'get_by_name',
		'get_action' => [ 'get_by_type_and_name', 'Server::Action' ],
		'list_actions' => [ 'get_by_type', 'Server::Action' ],
		'get_job' => [ 'get_by_type_and_name', 'Server::Job' ],
		'list_jobs' => [ 'get_by_type', 'Server::Job' ],
	},
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

	foreach my $job ($self->list_jobs) {
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

sub game_broadcast ($self, $location, $name, @args)
{
	$self->channel->broadcast($location, [$name, @args]);

	return;
}

sub broadcast_processable ($self, $processable, $session, @args)
{
	if ($processable isa 'Server::GameAction') {
		$self->game_broadcast($session->location, $processable->name, ($session->id, @args));
	}
	else {
		$self->broadcast($processable->name, ($session->id, @args));
	}
}

__END__

=pod

Worker is basically an internal server synchronizer and cron job scheduler. It
spawns processes, and then broadcasts jobs and actions to them. The
first child process to store a redis key with job / non-game action ULID gets to
process it. In case of actions, only one action at once will be listening to
certain location events.

Some processes work on cron-like and cleaup tasks (job processers), while
others take care of actual game logic. Each game location is held within one
process to take care of its events and keep its internal state, which is then
synchronized to the database in intervals.


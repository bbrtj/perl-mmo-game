package Server::Worker;

use My::Moose;
use Utils;
use Types;
use Server::Worker::Process;
use Data::ULID qw(ulid);
use Mojo::IOLoop;
use POSIX ();

use header;

$SIG{INT} = $SIG{KILL} = $SIG{TERM} = sub {
	Mojo::IOLoop->stop;
};

use constant PUBSUB_KEY => 'server_jobs';

has 'log' => (
	is => 'ro',
);

has 'redis' => (
	is => 'ro',
);

has 'encoder' => (
	is => 'ro',
);

has 'decoder' => (
	is => 'ro',
);

# Commands are internal only and ran in intervals
# (basically an advanced cron)
# TODO: intervals
has 'commands' => (
	is => 'ro',
	isa => Types::HashRef[Types::InstanceOf['Server::Command']],
	default => sub ($self) {
		return { map {
			$_->name => $_->new
		} grep { !$_->disabled } Utils->load_classes('Server::Command', 'Command/*.pm')
		};
	},
	init_arg => undef,
);

# Actions are events that need handling
# they are what players call to play the game
has 'actions' => (
	is => 'ro',
	isa => Types::HashRef[Types::InstanceOf['Server::Action']],
	default => sub ($self) {
		return { map {
			$_->name => $_->new
		} grep { !$_->disabled } Utils->load_classes('Server::Action', 'Action/*.pm')
		};
	},
	init_arg => undef,
);

sub start ($self, $processes = 4)
{
	my @children;
	for my $process_id (1 .. $processes) {
		my $pid = fork;
		if (defined $pid) {
			if ($pid) {
				Server::Worker::Process->new(worker => $self, process_id => $process_id)->do_work;
				exit;
			}
			else {
				$self->log->debug("Process $process_id started");
				push @children, $pid;
			}
		}
		else {
			$self->log->error("Could not fork worker ($process_id out of $processes)");
		}
	}

	my @commands = values $self->commands->%*;
	my $setup_commands = sub ($seconds) {
		for my $command (@commands) {
			if ($seconds % $command->interval == 0) {
				$self->broadcast(command => $command->name);
			}
		}
	};

	my $check_every = 1;
	my $interval; $interval = sub {
		state $seconds_passed = 0;
		$setup_commands->($seconds_passed);
		Mojo::IOLoop->timer($check_every => $interval);
		$seconds_passed += $check_every;
	};

	$interval->();
	Mojo::IOLoop->start;

	$self->log->debug('Worker is shutting down...');
	1 while waitpid(-1, POSIX::WNOHANG) > 0;
	return;
}

sub broadcast ($self, $type, $name, @args)
{
	my $data = [ulid, "$type:$name", @args];
	$self->redis->pubsub->notify(PUBSUB_KEY, $self->encoder->encode($data));

	return;
}


package Server::Worker;

use My::Moose;
use Types;
use Server::Worker::Process;
use Data::ULID qw(ulid);
use Mojo::IOLoop;
use Mojo::Loader qw(load_classes);

use header;

use constant PUBSUB_KEY => 'server_jobs';

with qw(
	Server::Forked
);

has 'channel' => (
	is => 'ro',
);

# Commands are internal only and ran in intervals
# (basically an advanced cron)
has 'commands' => (
	is => 'ro',
	isa => Types::HashRef [Types::InstanceOf ['Server::Command']],
	default => sub ($self) {
		return {
			map {
				$_->name => $_->new
			} grep { !$_->disabled } load_classes('Server::Command')
		};
	},
	init_arg => undef,
);

# Actions are events that need handling
# they are what players call to play the game
has 'actions' => (
	is => 'ro',
	isa => Types::HashRef [Types::InstanceOf ['Server::Action']],
	default => sub ($self) {
		return {
			map {
				$_->name => $_->new
			} grep { !$_->disabled } load_classes('Server::Action')
		};
	},
	init_arg => undef,
);

# keeps the worker alive in case of no specified commands
# can perform synchronization jobs
sub cleanup ($self)
{
	$self->log->info('Cleaning up...');
	Mojo::IOLoop->timer(60, sub { $self->cleanup });
}

sub start ($self, $processes = 2)
{
	$self->create_forks($processes, sub ($process_id) {
		Server::Worker::Process->new(worker => $self, process_id => $process_id)->do_work;
	});

	my $setup_command; $setup_command = sub ($command) {
		Mojo::IOLoop->timer($command->interval, sub {
			$self->broadcast(command => $command->name);
			$setup_command->($command);
		});
	};

	for my $command (values $self->commands->%*) {
		$setup_command->($command);
	}

	$self->cleanup;
	return;
}

sub broadcast ($self, $type, $name, @args)
{
	my $data = [ulid, "$type:$name", @args];
	$self->channel->broadcast(undef, $data);

	return;
}

__END__

=pod

Worker is basically an internal cron job runner and server synchronizer. It
spawns processes, and then broadcasts jobs (commands) to them. The first
process to store a redis key with job ULID gets to process it.

Some processes work on cron-like tasks, while others take care of actual game
logic. Each game zone spawns one process to take care of its events and keeps
its internal state, which is then synchronized to the database in intervals.


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

our $IS_WORKER = 0;

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

sub cleanup ($self)
{
	$self->log->info('Cleaning up...');
	Mojo::IOLoop->timer(60, sub { $self->cleanup });
}

sub start ($self, $processes = 2)
{
	local $IS_WORKER = 1;
	$self->create_forks($processes, sub ($process_id) {
		Server::Worker::Process->new(worker => $self, process_id => $process_id)->do_work;
	});

	my $broadcast_command = sub ($command) {
		$self->broadcast(command => $command->name);
	};

	my $setup_command; $setup_command = sub ($command) {
		Mojo::IOLoop->timer($command->interval, sub {
			$broadcast_command->($command);
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


package Server::Worker;

use My::Moose;
use Utils;
use Types;
use Server::Worker::Process;
use Data::ULID qw(ulid);
use Mojo::IOLoop;

use header;

use constant PUBSUB_KEY => 'server_jobs';

with qw(
	Server::Forked
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
	isa => Types::HashRef [Types::InstanceOf ['Server::Command']],
	default => sub ($self) {
		return {
			map {
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
	isa => Types::HashRef [Types::InstanceOf ['Server::Action']],
	default => sub ($self) {
		return {
			map {
				$_->name => $_->new
			} grep { !$_->disabled } Utils->load_classes('Server::Action', 'Action/*.pm')
		};
	},
	init_arg => undef,
);

sub start ($self, $processes = 2)
{
	$self->create_forks($processes, sub ($process_id) {
		Server::Worker::Process->new(worker => $self, process_id => $process_id)->do_work;
	});

	my @commands = values $self->commands->%*;
	my $setup_commands = sub ($seconds) {
		for my $command (@commands) {
			if ($seconds % $command->interval == 0) {
				$self->broadcast(command => $command->name);
			}
		}
	};

	my $check_every = 1;
	my $interval;
	$interval = sub {
		state $seconds_passed = 0;
		$setup_commands->($seconds_passed);
		Mojo::IOLoop->timer($check_every => $interval);
		$seconds_passed += $check_every;
	};

	$interval->();
	return;
}

sub broadcast ($self, $type, $name, @args)
{
	# TODO: this ulid may slow things down
	my $data = [ulid, "$type:$name", @args];
	$self->redis->pubsub->notify(PUBSUB_KEY, $self->encoder->encode($data));

	return;
}


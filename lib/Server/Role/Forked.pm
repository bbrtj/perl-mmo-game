package Server::Role::Forked;

use My::Moose::Role;
use Mojo::IOLoop;
use Utils;
use POSIX qw(WNOHANG);

use header;

requires qw(
	start
);

has injected 'log';

has field 'forked' => (
	isa => Types::Bool,
	writer => 1,
	default => !!0,
);

has field '_children' => (
	isa => Types::ArrayRef,
	default => sub { [] },
	'handles[]' => {
		children => 'all',
		add_child => 'push',
	}
);

# This will be handled by UV, but put it in there for other loops
## no critic
$SIG{INT} = sub {
	Mojo::IOLoop->stop;
};

after start => sub ($self, @) {

	# only parent gets this far

	Mojo::IOLoop->recurring(
		5 => sub {

			# TODO check if processes are okay
		},
	);

	Utils->handle_errors;
	Mojo::IOLoop->start;

	my @children = $self->children;
	my $try = 0;
	while (@children) {
		sleep 1;

		# children shallow copy, since it is modified inside the loop
		my @old_children = @children;
		foreach my $pid (@old_children) {
			my $dead_pid = waitpid($pid, WNOHANG);

			if ($dead_pid) {
				my $dead = $dead_pid > 0 ? $dead_pid : $pid;
				@children = grep { $_ != $pid } @children;
			}
		}

		kill 'INT', @children;
	}

	$self->log->info("Shutting down...");
};

sub _spawn ($self, $prefix, $processes, $after_fork //= sub { })
{
	foreach my $pnum (1 .. $processes) {
		my $process_id = "${prefix}${pnum}";

		my $pid = Utils->safe_fork;
		if (defined $pid) {
			$self->set_forked($pid == 0);

			# children returns here
			if ($self->forked) {
				return $process_id;
			}

			$self->log->info("Process $process_id started");
			$self->add_child($pid);
			$after_fork->();
		}
		else {
			$self->log->error("Could not fork process ($process_id out of $processes)");
		}
	}

	# parent returns here
	return "${prefix}0";
}

sub create_forks ($self, $prefix, $processes, $worker_code, $after_fork = undef)
{
	my $classname = ref $self;
	$self->log->system_name($classname);

	my $process_id = $self->_spawn($prefix, $processes, $after_fork);

	if ($self->forked) {
		local $0 = "perl $classname worker $process_id";
		$self->log->system_name("${classname}/${process_id}");
		Utils->handle_errors;

		$worker_code->($process_id);
		exit;
	}

	return;
}


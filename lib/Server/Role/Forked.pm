package Server::Role::Forked;

use My::Moose::Role;
use IO::Async::Timer::Periodic;
use POSIX qw(WNOHANG);

use header;

requires qw(
	start
);

has injected 'log';
has injected 'loop';

has field '_children' => (
	isa => ArrayRef,
	default => sub { [] },
	'handles[]' => {
		children => 'all',
		add_child => 'push',
	}
);

after start => sub ($self, @) {
	$self->loop->add(
		IO::Async::Timer::Periodic->new(
			interval => 5,
			reschedule => 'drift',
			on_tick => sub {

				# TODO check if processes are okay
			},
		)->start
	);

	local $SIG{INT} = sub { $self->loop->stop };
	$self->loop->run;

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

sub process_setup ($self)
{
	DI->get('redis')->connect($self->loop)->get;
}

sub create_forks ($self, $prefix, $processes, $worker_code, $after_fork //= sub { })
{
	my $classname = ref $self;
	$self->log->system_name($classname);

	foreach my $pnum (1 .. $processes) {
		my $process_id = "${prefix}${pnum}";

		my $pid = $self->loop->fork(
			code => sub {
				local $0 = "perl $classname worker $process_id";
				$self->log->system_name("${classname}/${process_id}");

				$self->process_setup;

				local $SIG{INT} = sub { $self->loop->stop };
				try {
					$worker_code->($process_id);
				}
				catch ($e) {
					$self->log->error($e);
					return 254;
				}

				return 0;
			},
			on_exit => sub ($pid, $code) {
				my $exitcode = $code >> 8;

				if ($exitcode == 0) {
					$self->log->info("$classname worker $process_id has ended");
				}
				else {
					$self->log->critical("$classname worker $process_id has died with code $exitcode");
				}
			},
		);

		$self->log->info("Process $process_id started");
		$self->add_child($pid);
		$after_fork->();
	}

	return;
}


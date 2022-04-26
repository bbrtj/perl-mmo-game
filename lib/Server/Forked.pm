package Server::Forked;

use My::Moose::Role;
use POSIX ();
use Mojo::IOLoop;

use header;

requires qw(
	start
);

has 'forked' => (
	is => 'rw',
	default => sub { 0 },
);

has 'log' => (
	is => 'ro',
);

sub setup ($self)
{
	$SIG{INT} = $SIG{KILL} = $SIG{TERM} = sub {
		Mojo::IOLoop->stop;
	};
}

after start => sub ($self, @) {
	Mojo::IOLoop->start;
	if (!$self->forked) {
		$self->log->info("Shutting down...");
		1 while waitpid(-1, POSIX::WNOHANG) > 0;
	}
};

sub create_forks ($self, $prefix, $processes, $worker_code, $parent_code = sub {})
{
	$self->setup;

	my $classname = ref $self;
	$self->log->system_name($classname);

	for my $pnum (1 .. $processes) {
		my $process_id = "${prefix}${pnum}";

		my $pid = fork;
		if (defined $pid) {
			if ($pid) {
				$self->forked(1);
				$0 = "perl $classname worker $process_id";
				$self->log->system_name("${classname}/${process_id}");

				$worker_code->($process_id);
				exit;
			}
			else {
				$self->log->info("Process $process_id started");
				$parent_code->();
			}
		}
		else {
			$self->log->error("Could not fork process ($process_id out of $processes)");
		}
	}

	return;
}


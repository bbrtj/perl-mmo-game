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
		$self->log->debug("Shutting down...");
		1 while waitpid(-1, POSIX::WNOHANG) > 0;
	}
};

sub create_forks ($self, $processes, $worker_code)
{
	$self->setup;

	my $classname = ref $self;
	my @children;
	for my $process_id (1 .. $processes) {
		my $pid = fork;
		if (defined $pid) {
			if ($pid) {
				$self->forked(1);
				$0 = "perl $classname worker #$process_id";

				$worker_code->($process_id);
				exit;
			}
			else {
				$self->log->debug("Process $process_id started");
				push @children, $pid;
			}
		}
		else {
			$self->log->error("Could not fork process ($process_id out of $processes)");
		}
	}

	return @children;
}

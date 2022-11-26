package Server::Role::Forked;

use My::Moose::Role;
use Mojo::IOLoop;
use Utils;

use header;

requires qw(
	start
);

has injected 'log';

has field 'forked' => (
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

sub setup ($self)
{
	foreach my $sig (qw(INT TERM)) {
		## no critic
		$SIG{$sig} = sub {
			kill $sig, $self->children;
			Mojo::IOLoop->stop;
		};
	}

	return $self;
}

after start => sub ($self, @) {
	Utils->handle_errors;
	Mojo::IOLoop->start;

	$self->log->info("Shutting down...");
	1 while wait != -1;
};

sub _spawn ($self, $prefix, $processes, $parent_code)
{
	foreach my $pnum (1 .. $processes) {
		my $process_id = "${prefix}${pnum}";

		my $pid = Utils->safe_fork;
		if (defined $pid) {
			my $is_parent = $pid > 0;

			$self->set_forked(1)
				unless $is_parent;

			# children returns here
			return $process_id unless $is_parent;

			$self->log->info("Process $process_id started");
			$self->add_child($pid);
			$parent_code->();
		}
		else {
			$self->log->error("Could not fork process ($process_id out of $processes)");
		}
	}

	# parent returns here
	return "${prefix}0";
}

sub create_forks ($self, $prefix, $processes, $worker_code, $parent_code = sub { })
{
	$self->setup;

	my $classname = ref $self;
	$self->log->system_name($classname);

	my $process_id = $self->_spawn($prefix, $processes, $parent_code);

	if ($self->forked) {
		local $0 = "perl $classname worker $process_id";
		$self->log->system_name("${classname}/${process_id}");

		$worker_code->($process_id);
		exit;
	}

	return;
}

sub create_forks_with_parent ($self, $prefix, $processes, $worker_code)
{
	$self->set_forked(1);
	$self->create_forks($prefix, $processes - 1, $worker_code);

	return;
}


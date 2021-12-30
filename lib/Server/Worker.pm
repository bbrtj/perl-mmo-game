package Server::Worker;

use Utils;

use header;

sub _make_task ($self, $class)
{
	my $command = $class->new;
	return sub {
		try {
			$command->handle(@_);
		}
		catch ($e) {
			DI->get('log')->logger->critical($e);
			die $e;
		}
	};
}

sub register ($self, $minion)
{
	# Commands are internal only
	foreach my $class (Utils->load_classes('Server::Worker::Command', 'Worker/Command/*.pm')) {
		$minion->add_task($class->name, $self->_make_task($class));
	}

	# Actions are what players call to play the game
	# (actions names are wrapped in braces so that we know which ones to permit)
	foreach my $class (Utils->load_classes('Server::Worker::Action', 'Worker/Action/*.pm')) {
		$minion->add_task('(' . $class->name . ')', $self->_make_task($class));
	}

	return;
}


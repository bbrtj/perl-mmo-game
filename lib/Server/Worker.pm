package Server::Worker;

use Utils;

use header;

sub register ($class, $minion)
{
	# Commands are internal only
	foreach my $class (Utils->load_classes('Server::Worker::Command', 'Worker/Command/*.pm')) {
		my $command = $class->new;
		$minion->add_task($command->name, sub { $command->handle(@_) });
	}

	# Actions are what players call to play the game
	# (actions names are wrapped in braces so that we know which ones to permit)
	foreach my $class (Utils->load_classes('Server::Worker::Action', 'Worker/Action/*.pm')) {
		my $command = $class->new;
		$minion->add_task('(' . $command->name . ')', sub { $command->handle(@_) });
	}

	return;
}


package Game::Worker;

use Game::Common;

use header;

sub register ($class, $minion)
{
	foreach my $class (Game::Common->load_classes('Game::Worker::Command', 'Worker/Command/*.pm')) {
		my $command = $class->new;
		$minion->add_task($command->name, $command->handler);
	}

	return;
}


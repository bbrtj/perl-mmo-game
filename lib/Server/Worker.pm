package Server::Worker;

use Utils;

use header;

sub register ($class, $minion)
{
	foreach my $class (Utils->load_classes('Server::Worker::Command', 'Worker/Command/*.pm')) {
		my $command = $class->new;
		$minion->add_task($command->name, $command->can('handler'));
	}

	return;
}


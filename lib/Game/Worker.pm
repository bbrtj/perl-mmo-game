package Game::Worker;

use Game::Common;

use header;

our $VERSION = "0.001";

sub register ($class, $minion)
{
	foreach my $class (Game::Common->load_classes('Game::Worker::Command', 'Worker/Command/*.pm')) {
		my $command = $class->new;
		$minion->add_task($command->get_name, $command->get_handler);
	}
}


package Game::Worker;

use Mojo::Base -signatures;
use File::Basename;

our $VERSION = "0.001";

sub register($class, $minion)
{
	my @commands = glob dirname(__FILE__) . '/Worker/Command/*.pm';
	my $namespace = 'Game::Worker::Command';

	foreach my $path (@commands) {
		my ($class) = $path =~ m{/Command/(.+).pm};

		$class = "${namespace}::${class}";
		eval "use $class";

		my $command = $class->new;
		$minion->add_task($command->get_name, $command->get_handler);
	}
}

1;

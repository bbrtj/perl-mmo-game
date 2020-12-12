package Game::Common;

use Mojo::Base -signatures;
use Mojolicious;

our $VERSION = "0.001";

sub get_config($class, $app, $file = undef)
{
	# Load configuration from config file
	my $config = $app->plugin('Config', file => $file);
	my $config_local = $app->plugin('Config', file => "$file.local");

	return { %$config, %$config_local };
}

1;

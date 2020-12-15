package Game::Common;

use Mojo::Base -signatures;
use Game::Common::Container qw(set_container);
use Mojo::Pg;

our $VERSION = "0.001";

sub _get_config($class, $app, $file)
{
	# Load configuration from config file
	my $config = $app->plugin('Config', file => $file);
	my $config_local = $app->plugin('Config', file => "$file.local");

	return { %$config, %$config_local };
}

sub bootstrap($class, $app, $config_file)
{
	my $config = $class->_get_config($app, $config_file);
	$app->plugin(Minion => {Pg => $config->{db}{connection}});
	my $pg = Mojo::Pg->new($config->{db}{connection});

	set_container(
		config => $config,
		pg => $pg,
		db => $pg->db,
		minion => $app->minion,
	);

	return;
}

1;

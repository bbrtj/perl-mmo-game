package Game::Bootstrap;

use header;
use Exporter qw(import);
use Game::Common;
use Game::Schema;
use Game::Model;
use Game::Repository;
use Game::Common::Container qw(set_container);
use Mojo::Pg;
use Mojo::File qw(curfile);

our @EXPORT = qw(
	bootstrap
);

sub bootstrap ($app)
{
	my $config = Game::Common->get_config($app, curfile->dirname->to_string . '/../../vars.conf');
	$app->plugin(Minion => {Pg => $config->{db}{connection}});
	my $pg = Mojo::Pg->new($config->{db}{connection});

	set_container(
		pg => $pg,
		db => $pg->db,
		minion => $app->minion,
	);

	Game::Schema->bootstrap;
	Game::Model->bootstrap;
	Game::Repository->bootstrap;

	return $config;
}

1;


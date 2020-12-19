package Game::Bootstrap;

use header;
use Exporter qw(import);
use Game::Common;
use Game::Schema;
use Game::Common::Container qw(set_container);
use Mojo::Pg;
use Mojo::File qw(curfile);
use Game::Data::Repository;
use Game::Cache::Repository;

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
		game_data_repo => Game::Data::Repository->new,
		cache_repo => Game::Cache::Repository->new,
	);

	Game::Schema->bootstrap;
	Game::Model->bootstrap;

	return $config;
}

1;


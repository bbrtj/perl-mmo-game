package Game::Bootstrap;

use Exporter qw(import);
use Schema;
use Game::Common;
use Model;
use Mojo::Pg;
use Mojo::File qw(curfile);
use DI;

use header -noclean;

our @EXPORT = qw(
	bootstrap
);

sub bootstrap ($app)
{
	my $config = DI->get('env');
	$app->plugin(Minion => {Pg => $config->getenv('DB_CONNECTION')});
	my $pg = Mojo::Pg->new($config->getenv('DB_CONNECTION'));

	Model->bootstrap;

	return $config;
}


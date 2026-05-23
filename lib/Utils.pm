package Utils;

use Game::LoreLoader;

use header;

sub safe_fork ($class)
{
	DI->get('db')->clear_dbh;
	return fork;
}


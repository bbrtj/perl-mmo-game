package Game::Helpers;

use Game::LoreLoader;
use Utils qw(pascal_case);
use Sub::Install;
use Exporter qw(import);

use header;

our @EXPORT = map { "lore_$_" } Game::LoreLoader->LORE_TYPES->@*;
our @EXPORT_OK = ();

foreach my $type (Game::LoreLoader->LORE_TYPES->@*) {
	my $class = 'Game::Lore::' . pascal_case($type);

	Sub::Install::install_sub(
		{
			as => "lore_$type",
			code => sub :prototype($) ($name) {
				state $repo = DI->get('lore_data_repo');
				return $repo->load_named($class, $name);
			},
		}
	);
}


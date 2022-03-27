package Game::Helpers;

use Game::LoreLoader::DSL -helpers;
use Exporter qw(import);

use header -noclean;

our @EXPORT = map { "lore_$_" } Game::LoreLoader::DSL->TYPES->@*;
our @EXPORT_OK = ();


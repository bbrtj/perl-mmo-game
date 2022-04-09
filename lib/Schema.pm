package Schema;

use Mojo::Loader;

use header;

use parent qw(DBIx::Class::Schema);

# DBIx::Class::Schema got a method named load_classes as well
Mojo::Loader::load_classes('Schema');


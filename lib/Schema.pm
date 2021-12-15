package Schema;

use DI;

use header;

use parent qw(DBIx::Class::Schema);

our $VERSION = "0.001";

__PACKAGE__->load_namespaces();


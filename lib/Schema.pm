package Schema;

use header;

use parent qw(DBIx::Class::Schema);

# use this instead of schema class loading
# so that resultsets are not automatically registered
use all 'Schema';


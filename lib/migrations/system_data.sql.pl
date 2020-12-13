use Mojo::File qw(curfile path);
use Encode qw(decode);

my @files = glob curfile->dirname->to_string . '/system_data/*.sql';
my $contents = join "\n", map { decode("UTF-8", path($_)->slurp) } @files;

# replication requires superuser access
return <<SQL;

-- 4 up

SET session_replication_role = 'replica';

$contents

SET session_replication_role = 'origin';

-- 4 down

SQL

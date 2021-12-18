use Mojo::File qw(curfile path);
use Encode qw(decode);

use strict;
use warnings;

my @files = map { curfile->dirname->to_string . "/system_data/$_.sql" } qw(
	config
);

my $contents = join "\n", map { decode("UTF-8", path($_)->slurp) } @files;

return <<SQL;

-- 4 up

$contents

-- 4 down

SQL

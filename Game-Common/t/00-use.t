use v5.32;
use warnings;

use Test::More;

# test compilation of significant system parts
use_ok('Game::Common');
use_ok('Game::Common::Container');

use header;
ok defined $_isa, 'isa ok';

done_testing;

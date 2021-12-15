use Test::More;
use Test::Mojo;

use header -noclean;

my $t = Test::Mojo->new('Game');
$t->get_ok('/')->status_is(200);

done_testing();

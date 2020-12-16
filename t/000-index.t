use v5.30; use warnings;

use Test::More;
use Test::Mojo;

my $t = Test::Mojo->new('Game');
$t->get_ok('/')->status_is(200);

done_testing();

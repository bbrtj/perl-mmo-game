use v5.30;
use warnings;

use Test::More;
use Game::Ability::Parser;
use lib 't/lib';
use DatabaseTest;

DatabaseTest->test(
	sub {
		my $parsed = Game::Ability::Parser->parse;
		ok exists $parsed->{ABI_STRIKE};
	}
);

done_testing;

package Game::Repository::BattleUnit;

use header;
use Moo;
use Game::Common::Container;
use Game::Types qw(ConsumerOf);

no header;

with 'Game::Repository::Role::Resource';

sub save ($self, $unit)
{
	state $type_check = ConsumerOf ['Game::Unit::Battle'];
	$type_check->assert_valid($unit);

	# TODO
}

sub load ($self, $id)
{
	# TODO
}

1;

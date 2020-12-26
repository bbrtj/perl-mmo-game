package Game::RepositoryBase;

use header;
use Moo;
use Game::Types qw(ConsumerOf);

no header;

has [
	qw(
		char_cache
		ability_data
		class_data
		lore_data
		schema
		actor_unit
		battle_unit
		)
] => (
	is => 'ro',
	isa => ConsumerOf ['Game::Repository::Role::Resource'],
);

1;

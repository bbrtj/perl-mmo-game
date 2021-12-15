package Game::Repository::LoreData;

use Moo;
use DI;
use Types;

use header;

with 'Game::Repository::Role::Resource';

sub save { ... }

# TODO lang
sub load ($self, $type, $id, $lang = 'PL')
{
	state $check = Types::Enum [qw(name description)];
	$check->assert_valid($type);

	my $result = DI->get('db')->db->query(<<~"SQL", $id, $lang)->hash;
		SELECT $type FROM gd_lore_${type}s WHERE lore_id = ? AND language_id = ?
	SQL

	die "no lore $type for id `$id` and language `$lang`"
		unless $result;

	return $result->{$type};
}


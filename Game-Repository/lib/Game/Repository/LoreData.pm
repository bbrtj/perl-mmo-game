package Game::Repository::LoreData;

use header;
use Moo;
use Game::Common::Container;
use Game::Types qw(Enum);

no header;

with 'Game::Repository::Role::Resource';

sub save { ... }

# TODO lang
sub load ($self, $type, $id, $lang = 'PL')
{
	state $check = Enum [qw(name description)];
	$check->assert_valid($type);

	my $result = resolve('db')->query(<<~"SQL", $id, $lang)->hash;
		SELECT $type FROM gd_lore_${type}s WHERE lore_id = ? AND language_id = ?
	SQL

	die "no lore $type for id `$id` and language `$lang`"
		unless $result;

	return $result->{$type};
}

1;

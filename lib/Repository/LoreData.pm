package Repository::LoreData;

use My::Moose;
use Types;

use header;

with 'Repository::Role::Resource';

has 'db' => (
	is => 'ro',
);

sub save { ... }

sub load ($self, $type, $id, $lang)
{
	state $check = Types::Enum [qw(name description)];
	$check->assert_valid($type);

	my $result = $self->db->db->query(<<~"SQL", $id, uc $lang)->hash;
		SELECT $type FROM gd_lore_${type}s WHERE lore_id = ? AND language_id = ?
	SQL

	die "no lore $type for id `$id` and language `$lang`"
		unless $result;

	return $result->{$type};
}


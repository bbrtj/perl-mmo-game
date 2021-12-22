package Repository::CharCache;

use Moo;
use Mojo::JSON qw(decode_json encode_json);

use header;

with 'Repository::Role::Resource';

has 'db' => (
	is => 'ro',
);

sub save ($self, $id, $data = {})
{
	$data = {id => $id, data => encode_json($data)};
	return $self->db->db->insert(
		'character_cache', $data,
		{on_conflict => [id => $data]}
	);
}

sub load ($self, $id)
{
	my $cache = $self->db->db->select('character_cache', undef, {id => $id})
		->hash;

	return $cache ? decode_json($cache->{data}) : {};
}


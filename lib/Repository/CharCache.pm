package Repository::CharCache;

use Moo;
use DI;
use Mojo::JSON qw(decode_json encode_json);

use header;

with 'Repository::Role::Resource';

sub save ($self, $id, $data = {})
{
	my $db = DI->get('db')->db;
	$data = {id => $id, data => encode_json($data)};
	return $db->insert(
		'character_cache', $data,
		{on_conflict => [id => $data]}
	);
}

sub load ($self, $id)
{
	my $db = DI->get('db')->db;
	my $cache = $db->select('character_cache', undef, {id => $id})
		->hash;

	return $cache ? decode_json($cache->{data}) : {};
}


package Game::Repository::CharCache;

use header;
use Moo;
use Game::Common::Container;
use JSON::MaybeXS;

no header;

with 'Game::Repository::Role::Resource';

sub save ($self, $id, $data = {})
{
	my $db = resolve('db');
	$data = {id => $id, data => encode_json($data)};
	return $db->insert(
		'character_cache', $data,
		{on_conflict => [id => $data]}
	);
}

sub load ($self, $id)
{
	my $db = resolve('db');
	my $cache = $db->select('character_cache', undef, {id => $id})
		->hash;

	return $cache ? decode_json($cache->{data}) : {};
}

1;

package Repository::CharCache;

use Moo;
use Mojo::JSON qw(decode_json encode_json);

use header;

with 'Repository::Role::Resource';

use constant REDIS_KEY => 'char_cache';

has 'redis' => (
	is => 'ro',
);

sub save ($self, $id, $data = {})
{
	$self->redis->db->hset(REDIS_KEY, $id, encode_json($data));
	return 1;
}

sub load ($self, $id)
{
	my $cache = $self->redis->db->hget(REDIS_KEY, $id);

	return $cache ? decode_json($cache) : {};
}


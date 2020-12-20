package Game::Repository::CharCache;

use header;
use Moo;
use Game::Common::Container;

no header;

with 'Game::Repository::Role::Resource';

sub save ($self, $data)
{
	my $db = resolve('db');
	return $db->insert(
		'character_calculations', $data,
		{on_conflict => [id => $data]}
	);
}

sub load ($self, $id)
{
	my $db = resolve('db');
	return $db->select('character_calculations', undef, {id => $id})
		->hash;
}

1;

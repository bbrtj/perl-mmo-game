package Game::Cache::Repository;

use header;
use Moo;
use Game::Common::Container;

no header;

package Inlined::Repository::CharacterCalc {
	use Moo;
	no header;

	sub load ($self, $id)
	{
		my $db = resolve('db');
		return $db->select('character_calculations', undef, {id => $id});
	}

	sub save ($self, $data)
	{
		my $db = resolve('db');
		return $db->insert(
			'character_calculations', $data,
			{on_conflict => [id => {map { $_ => $data->{$_} } grep { $_ ne 'id' } $data}]}
		);
	}
}

sub character_calc ($class)
{
	return Inlined::Repository::CharacterCalc->new;
}

1;

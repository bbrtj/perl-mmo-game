package Game::Cache::Repository;

use header;
use Moo;
use Game::Common::Container;

no header;

package Inlined::Repository::CharacterCalc {
	use Moo;

	sub load ($self, $id)
	{
		my $db = resolve('db');
		my $data = $db->select('character_calculations', undef, {id => $id});

		return $data;
	}

	sub save ($self, $data)
	{
		my $db = resolve('db');
		my $data = $db->insert(
			'character_calculations', $data,
			{on_conflict => [id => {map { $_ => $data->{$_} } grep { $_ ne 'id' } $data}]}
		);

		return $data;
	}
}

sub character_calc ($class)
{
	return Inlined::Repository::CharacterCalc->new;
}

1;

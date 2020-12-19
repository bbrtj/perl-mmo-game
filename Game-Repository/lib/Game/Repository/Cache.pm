package Game::Repository::Cache;

use header;
use Moo;
use Game::Common::Container;

no header;

package Game::Repository::Cache::CharCalc {
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

sub char_calc ($class)
{
	return Game::Repository::Cache::CharCalc->new;
}

1;

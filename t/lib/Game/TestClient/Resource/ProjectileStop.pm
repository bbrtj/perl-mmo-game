use experimental 'class';

class Game::TestClient::Resource::ProjectileStop :isa(Resource::ProjectileStop);

use header;

field $id :writer;

method mangle_for_test ($real_data, $expected_data)
{
	die 'no id set in ProjectileStop'
		unless defined $id;

	$expected_data = $self->deserialize($expected_data);
	$expected_data->[0] = $id;
	return $self->serialize($expected_data);
}


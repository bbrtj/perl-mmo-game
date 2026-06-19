use experimental 'class';

class Game::TestClient::Resource::Projectile :isa(Resource::Projectile);

use header;

field $stop_resource :param = undef;    # Game::TestClient::Resource::ProjectileStop

method mangle_for_test ($real_data, $expected_data)
{
	$expected_data = $self->deserialize($expected_data);
	$real_data = $self->deserialize($real_data);
	my $id = $real_data->[0];
	$expected_data->[0] = $id;

	$stop_resource->set_id($id)
		if $stop_resource;
	return $self->serialize($expected_data);
}


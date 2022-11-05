package Game::Server;

use My::Moose;

use header;

has param 'process' => (
	isa => Types::InstanceOf['Server::Process::Game'],
	weak_ref => 1,
	'handles->' => {
		'send_to' => 'send_to',
		'log' => 'log',
	},
);

has param 'location_data' => (
	isa => Types::InstanceOf ['Unit::Location'],
);

sub tick ($self, $elapsed)
{
	foreach (1 .. 1000) {
		Model::Player->new(user_id => 'a' x 26);
	}
}


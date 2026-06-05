package Game::TestClient::Action::Login;

use My::Moose;
use TestHelpers;

use header;

extends 'Game::TestClient::Action';

has param 'user' => (
	isa => InstanceOf ['Model::User'],
);

has param 'password' => (
	isa => SimpleStr,
);

sub send_queue ($self)
{
	return (
		['login', __serialize({email => $self->user->email, password => hash_password($self->password)})],
	);
}

sub receive_queue ($self)
{
	return (
		'1',
	);
}


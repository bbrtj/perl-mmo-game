package Game::TestClient::Action::Login;

use My::Moose;

use header;

extends 'Game::TestClient::Action';

has param 'user' => (
	isa => Types::InstanceOf ['Model::User'],
);

has param 'password' => (
	isa => Types::SimpleStr,
);

sub send_queue ($self)
{
	return (
		['login', __serialize({email => $self->user->email, password => $self->password})],
	);
}

sub receive_queue ($self)
{
	return (
		'1',
	);
}


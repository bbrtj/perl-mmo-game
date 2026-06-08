package Game::TestClient::Action::Feed;

use My::Moose;

use header;

extends 'Game::TestClient::Action';

has param 'data';

use constant sequential => false;

sub send_queue ($self)
{
	return ();
}

sub receive_queue ($self)
{
	my $data = $self->data;

	return ($data)
		unless ref $data eq 'ARRAY';

	return ($data->@*);
}


package Game::TestClient::Action::Feed;

use My::Moose;

use header;

extends 'Game::TestClient::Action';

has param 'data';

use constant sequential => !!0;

sub send_queue ($self)
{
	return ();
}

sub receive_queue ($self)
{
	my $data = $self->data;

	return ($data)
		unless is_arrayref $data;

	return ($data->@*);
}


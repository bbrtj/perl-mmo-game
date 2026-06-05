package Game::TestClient::Action::State;

use My::Moose;
use My::Dumper;
use Value::Diff;

use header;

extends 'Game::TestClient::Action';

has param 'received' => (
	isa => HashRef,
	writer => 1,
);

has param 'types' => (
	isa => ArrayRef,
);

use constant sequential => false;
use constant requires => ['EnterGame'];

sub send_queue ($self)
{
	return ();
}

sub receive_queue ($self)
{
	return ();
}

sub finished ($self)
{
	return !$self->received->%*;
}

sub should_send ($self)
{
	return false;
}

sub find_and_compare ($self, $type, $data)
{
	try {
		$data = __deserialize($data);
	}
	catch ($e) {
		return false;
	}

	if (diff($data, $self->received)) {
		return false;
	}

	if (!any { $_ eq $type } $self->types->@*) {
		return false;
	}

	diff($self->received, $data, \my $diff);
	$self->set_received($diff);

	return true;
}

sub get_expected_type ($self)
{
	return My::Dumper->ddshort($self->types);
}

sub get_expected_data ($self)
{
	return My::Dumper->dd($self->received);
}


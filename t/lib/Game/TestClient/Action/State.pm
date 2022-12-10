package Game::TestClient::Action::State;

use My::Moose;
use My::Dumper;
use Value::Diff;

use header;

extends 'Game::TestClient::Action';

has param 'received' => (
	isa => Types::HashRef,
	writer => 1,
);

has param 'types' => (
	isa => Types::ArrayRef,
);

use constant sequential => !!0;
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
	return !!0;
}

sub find_and_compare ($self, $type, $data)
{
	$data = $self->decode($data);

	if (diff($data, $self->received)) {
		return !!0;
	}

	if (!any { $_ eq $type } $self->types->@*) {
		return !!0;
	}

	diff($self->received, $data, \my $diff);
	$self->set_received($diff);

	return !!1;
}

sub get_expected_type ($self)
{
	return My::Dumper->ddshort($self->types);
}

sub get_expected_data ($self)
{
	return My::Dumper->dd($self->received);
}


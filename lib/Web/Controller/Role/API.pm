package Web::Controller::Role::API;

use My::Moose::Role;
use JSON::MaybeXS qw(decode_json);

use header;

requires qw(req);

sub get_input ($self)
{
	return decode_json($self->req->body);
}

sub respond ($self, $status, $data)
{
	my %ret = (
		status => $status,
		data => $data,
	);

	return $self->render(json => \%ret);
}


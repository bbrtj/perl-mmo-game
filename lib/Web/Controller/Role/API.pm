package Web::Controller::Role::API;

use My::Moose::Role;
use Mojo::JSON qw(decode_json false true);

use header;

requires qw(req);

sub get_input ($self)
{
	return decode_json($self->req->body);
}

sub respond ($self, $status, $data)
{
	my %ret = (
		status => $status ? true : false,
		data => $data,
	);

	return $self->render(json => \%ret);
}

